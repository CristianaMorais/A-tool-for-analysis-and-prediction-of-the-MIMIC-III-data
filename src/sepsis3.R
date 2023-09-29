library(DBI)
library(bigrquery)

setwd("~/Github/MIMIC-III")

con <- dbConnect(bigquery(),
                 project = "physionet-data",
                 billing = "clinical-entity-extraction"
)

#conClinical <- dbConnect(
#  bigrquery::bigquery(),
#  project = "physionet-data",
#  dataset = "mimiciii_clinical",
#  billing = "clinical-entity-extraction"
#)

#conDerived <- dbConnect(
#  bigrquery::bigquery(),
#  project = "physionet-data",
#  dataset = "mimiciii_derived",
#  billing = "clinical-entity-extraction"
#)

bq_auth(email = "up201505454@g.uporto.pt")

# Get the sepsis-3 pacients
sepsis3_alistairewj <- "WITH abx_p as (
                select pr.hadm_id
                , pr.drug as antibiotic_name
                , pr.startdate as antibiotic_time
                , pr.enddate as antibiotic_endtime
                from `physionet-data.mimiciii_clinical.prescriptions` pr
                -- inner join to subselect to only antibiotic prescriptions
                inner join `physionet-data.mimiciii_derived.abx_poe_list` ab
                on pr.drug = ab.drug
              ),

          ab_tbl as (
            select
            ie.subject_id, ie.hadm_id, ie.icustay_id
            , ie.intime, ie.outtime
            , abx_p.antibiotic_name
            , abx_p.antibiotic_time
            , abx_p.antibiotic_endtime
            from `physionet-data.mimiciii_clinical.icustays` ie
            left join abx_p
            on ie.hadm_id = abx_p.hadm_id
          ),
          
          me as (
            select hadm_id
            , chartdate, charttime
            , spec_type_desc
            , max(case when org_name is not null and org_name != '' then 1 else 0 end) as PositiveCulture
            from `physionet-data.mimiciii_clinical.microbiologyevents`
            group by hadm_id, chartdate, charttime, spec_type_desc
          ),
          
          ab_fnl as (
            select
            ab_tbl.icustay_id, ab_tbl.intime, ab_tbl.outtime
            , ab_tbl.antibiotic_name
            , ab_tbl.antibiotic_time
            , coalesce(me72.charttime,me72.chartdate) as last72_charttime
            , coalesce(me24.charttime,me24.chartdate) as next24_charttime
            
            , me72.positiveculture as last72_positiveculture
            , me72.spec_type_desc as last72_specimen
            , me24.positiveculture as next24_positiveculture
            , me24.spec_type_desc as next24_specimen
            from ab_tbl
            -- blood culture in last 72 hours
            left join me me72
            on ab_tbl.hadm_id = me72.hadm_id
            and ab_tbl.antibiotic_time is not null
            and
            (
              -- if charttime is available, use it
              (
                ab_tbl.antibiotic_time > me72.charttime
                and ab_tbl.antibiotic_time <= me72.charttime + interval '72' hour
              )
              OR
              (
                -- if charttime is not available, use chartdate
                me72.charttime is null
                and ab_tbl.antibiotic_time > me72.chartdate
                and ab_tbl.antibiotic_time < me72.chartdate + interval '96' hour -- could equally do this with a date_trunc, but that's less portable
                                )
                    )
                  -- blood culture in subsequent 24 hours
                  left join me me24
                    on ab_tbl.hadm_id = me24.hadm_id
                    and ab_tbl.antibiotic_time is not null
                    and me24.charttime is not null
                    and
                    (
                      -- if charttime is available, use it
                      (
                          ab_tbl.antibiotic_time > me24.charttime - interval '24' hour
                      and ab_tbl.antibiotic_time <= me24.charttime
                      )
                      OR
                      (
                      -- if charttime is not available, use chartdate
                          me24.charttime is null
                      and ab_tbl.antibiotic_time > me24.chartdate
                      and ab_tbl.antibiotic_time <= me24.chartdate + interval '24' hour
                      )
                    )
                ),
                
              ab_laststg as
                  (
                  select
                    icustay_id
                    , antibiotic_name
                    , antibiotic_time
                    , last72_charttime
                    , next24_charttime
                  
                    -- time of suspected infection: either the culture time (if before antibiotic), or the antibiotic time
                    , case
                        when coalesce(last72_charttime,antibiotic_time) is null
                          then 0
                        else 1 end as suspected_infection
                  
                    , coalesce(last72_charttime,antibiotic_time) as suspected_infection_time
                  
                    -- the specimen that was cultured
                    , case
                        when last72_charttime is not null
                          then last72_specimen
                        when next24_charttime is not null
                          then next24_specimen
                      else null
                    end as specimen
                  
                    -- whether the cultured specimen ended up being positive or not
                    , case
                        when last72_charttime is not null
                          then last72_positiveculture
                        when next24_charttime is not null
                          then next24_positiveculture
                      else null
                    end as positiveculture
                  from ab_fnl
                  ),

              abx_micro_poe as (
                 select icustay_id
                  , antibiotic_name
                  , antibiotic_time
                  , last72_charttime
                  , next24_charttime
                  , suspected_infection_time
                  -- -- the below two fields are used to extract data - modifying them facilitates sensitivity analyses
                  -- , suspected_infection_time - interval '48' hour as si_starttime
                  -- , suspected_infection_time + interval '24' hour as si_endtime
                  , specimen, positiveculture
                from ab_laststg
                order by icustay_id, antibiotic_time
    
              ),

              abx as (
                select icustay_id
                  , suspected_infection_time
                  , specimen, positiveculture
                  , antibiotic_name
                  , antibiotic_time
                  , ROW_NUMBER() OVER
                  (
                    PARTITION BY icustay_id
                    ORDER BY suspected_infection_time
                  ) as rn
                from abx_micro_poe
              ),
            
              suspinfect_poe as (
                 select ie.icustay_id
                  , antibiotic_name
                  , antibiotic_time
                  , suspected_infection_time
                  , specimen, positiveculture
                from `physionet-data.mimiciii_clinical.icustays` ie
                left join abx
                  on ie.icustay_id = abx.icustay_id
                  and abx.rn = 1
                order by ie.icustay_id
              ),
              
              serv AS (
                select hadm_id, curr_service
                  , ROW_NUMBER() over (partition by hadm_id order by transfertime) as rn
                from `physionet-data.mimiciii_clinical.services`
              ),
                
              t1 AS (
                select ie.subject_id, ie.icustay_id, ie.hadm_id
                  , ie.intime, ie.outtime
                  , DATETIME_DIFF(adm.admittime, pat.dob, YEAR) AS age
                  , pat.gender
                  , adm.ethnicity
                  , ie.dbsource
                  -- used to get first ICUSTAY_ID
                  , ROW_NUMBER() over (partition by ie.subject_id order by intime) as rn
                  
                  -- exclusions
                  , s.curr_service as first_service
                  , adm.HAS_CHARTEVENTS_DATA
                  
                  -- suspicion of infection using POE
                  , case when spoe.suspected_infection_time is not null then 1 else 0 end
                      as suspected_of_infection_poe
                  , spoe.suspected_infection_time as suspected_infection_time_poe
                  , DATETIME_DIFF(ie.intime, spoe.suspected_infection_time, DAY) /60.0 / 60.0 / 24.0 as suspected_infection_time_poe_days
                  , spoe.specimen as specimen_poe
                  , spoe.positiveculture as positiveculture_poe
                  , spoe.antibiotic_time as antibiotic_time_poe
                  
                  from `physionet-data.mimiciii_clinical.icustays` ie
                  inner join `physionet-data.mimiciii_clinical.admissions` adm
                      on ie.hadm_id = adm.hadm_id
                  inner join `physionet-data.mimiciii_clinical.patients` pat
                      on ie.subject_id = pat.subject_id
                  left join serv s
                      on ie.hadm_id = s.hadm_id
                      and s.rn = 1
                  left join suspinfect_poe spoe
                       on ie.icustay_id = spoe.icustay_id
              ),
              
              sepsis3_cohort AS (
              
                  select
                      t1.subject_id, t1.hadm_id, t1.icustay_id
                    , t1.intime, t1.outtime
                  
                    -- set de-identified ages to median of 91.4
                    , case when age > 89 then 91.4 else age end as age
                    , gender
                    , ethnicity
                    , first_service
                    , dbsource
                  
                    -- suspicion using POE
                    , suspected_of_infection_poe
                    , suspected_infection_time_poe
                    , suspected_infection_time_poe_days
                    , specimen_poe
                    , positiveculture_poe
                    , antibiotic_time_poe
                  
                    -- exclusions
                    , case when t1.rn = 1 then 0 else 1 end as exclusion_secondarystay
                    , case when t1.age <= 16 then 1 else 0 end as exclusion_nonadult
                    , case when t1.first_service in ('CSURG','VSURG','TSURG') then 1 else 0 end as exclusion_csurg
                    , case when t1.dbsource != 'metavision' then 1 else 0 end as exclusion_carevue
                    , case when t1.suspected_infection_time_poe is not null
                            and t1.suspected_infection_time_poe < (t1.intime-interval '1' day) then 1
                        else 0 end as exclusion_early_suspicion
                    , case when t1.suspected_infection_time_poe is not null
                            and t1.suspected_infection_time_poe > (t1.intime+interval '1' day) then 1
                        else 0 end as exclusion_late_suspicion
                    , case when t1.HAS_CHARTEVENTS_DATA = 0 then 1
                           when t1.intime is null then 1
                           when t1.outtime is null then 1
                        else 0 end as exclusion_bad_data
                    -- , case when t1.suspected_of_infection = 0 then 1 else 0 end as exclusion_suspicion
                  
                    -- the above flags are used to summarize patients excluded
                    -- below flag is used to actually exclude patients in future queries
                    , case when
                               t1.rn != 1
                            or t1.age <= 16
                            or t1.first_service in ('CSURG','VSURG','TSURG')
                            or t1.HAS_CHARTEVENTS_DATA = 0
                            or t1.intime is null
                            or t1.outtime is null
                            or t1.dbsource != 'metavision'
                            or (
                                    t1.suspected_infection_time_poe is not null
                                and t1.suspected_infection_time_poe < (t1.intime-interval '1' day)
                              )
                            or (
                                    t1.suspected_infection_time_poe is not null
                                and t1.suspected_infection_time_poe > (t1.intime+interval '1' day)
                              )
                            -- or t1.suspected_of_infection = 0
                              then 1
                          else 0 end as excluded
                  from t1
                  order by t1.icustay_id
              
              ),

              vitals AS (
              SELECT pvt.icustay_id
                -- Easier names
                , min(case when VitalID = 2 then valuenum else null end) as SysBP_Min
                , max(case when VitalID = 5 then valuenum else null end) as RespRate_Max
                FROM (
                  select ie.icustay_id
                  , case
                    when itemid in (51,442,455,6701,220179,220050) and valuenum > 0 and valuenum < 400 then 2 -- SysBP
                    when itemid in (615,618,220210,224690) and valuenum > 0 and valuenum < 70 then 5 -- RespRate
                    else null end as VitalID
                  -- convert F to C
                  , valuenum
                from `physionet-data.mimiciii_clinical.icustays` ie
                left join `physionet-data.mimiciii_clinical.chartevents` ce
                on ie.icustay_id = ce.icustay_id
                and ce.charttime
                between ie.intime - interval '6' hour
                  and ie.intime + interval '6' hour
                  and ce.itemid in (
                  -- Systolic/diastolic
                  51, --	Arterial BP [Systolic]
                  442, --	Manual BP [Systolic]
                  455, --	NBP [Systolic]
                  6701, --	Arterial BP #2 [Systolic]
                  220179, --	Non Invasive Blood Pressure systolic
                  220050, --	Arterial Blood Pressure systolic
                    
                  -- RESPIRATORY RATE
                  618,--	Respiratory Rate
                  615,--	Resp Rate (Total)
                  220210,--	Respiratory Rate
                  224690 --	Respiratory Rate (Total)
                )
                -- exclude rows marked as error
                AND ce.error IS DISTINCT FROM 1
              ) 
              pvt group by pvt.icustay_id
            ),
            
            -- GCS
            base AS (
              SELECT pvt.ICUSTAY_ID
                , pvt.charttime
                -- Easier names - note we coalesced Metavision and CareVue IDs below
                , max(case when pvt.itemid = 454 then pvt.valuenum else null end) as GCSMotor
                , max(case when pvt.itemid = 723 then pvt.valuenum else null end) as GCSVerbal
                , max(case when pvt.itemid = 184 then pvt.valuenum else null end) as GCSEyes
                      
                 -- If verbal was set to 0 in the below select, then this is an intubated patient
                 , case
                    when max(case when pvt.itemid = 723 then pvt.valuenum else null end) = 0
                     then 1
                    else 0
                     end as EndoTrachFlag
                   
                 , ROW_NUMBER ()
                     OVER (PARTITION BY pvt.ICUSTAY_ID ORDER BY pvt.charttime ASC) as rn
                   
               FROM (
                 select l.ICUSTAY_ID
                        -- merge the ITEMIDs so that the pivot applies to both metavision/carevue data
                        , case
                            when l.ITEMID in (723,223900) then 723
                            when l.ITEMID in (454,223901) then 454
                            when l.ITEMID in (184,220739) then 184
                            else l.ITEMID end
                          as ITEMID
                      
                        -- convert the data into a number, reserving a value of 0 for ET/Trach
                        , case
                            -- endotrach/vent is assigned a value of 0, later parsed specially
                            when l.ITEMID = 723 and l.VALUE = '1.0 ET/Trach' then 0 -- carevue
                            when l.ITEMID = 223900 and l.VALUE = 'No Response-ETT' then 0 -- metavision
                      
                            else VALUENUM
                            end
                          as VALUENUM
                        , l.CHARTTIME
                        from `physionet-data.mimiciii_clinical.icustays` ie
                        left join `physionet-data.mimiciii_clinical.chartevents` l
                          on l.icustay_id = ie.icustay_id
                          -- Only get data for the first 6 hours
                          and l.charttime
                          between ie.intime - interval '6' hour
                              and ie.intime + interval '6' hour
                          -- exclude rows marked as error
                          AND l.error IS DISTINCT FROM 1
                          -- Isolate the desired GCS variables
                          and l.ITEMID in
                          (
                            -- 198 -- GCS
                            -- GCS components, CareVue
                            184, 454, 723
                            -- GCS components, Metavision
                            , 223900, 223901, 220739
                          )
                        ) pvt
                        group by pvt.ICUSTAY_ID, pvt.charttime
            ),
            
            gcs AS (
                        select b.icustay_id
                        -- Calculate GCS, factoring in special case when they are intubated and prev vals
                        -- note that the coalesce are used to implement the following if:
                        --  if current value exists, use it
                        --  if previous value exists, use it
                        --  otherwise, default to normal
                        , min(case
                            -- replace GCS during sedation with 15
                            when b.GCSVerbal = 0
                              then 15
                            when b.GCSVerbal is null and b2.GCSVerbal = 0
                              then 15
                            -- if previously they were intub, but they aren't now, do not use previous GCS values
                            when b2.GCSVerbal = 0
                              then
                                  coalesce(b.GCSMotor,6)
                                + coalesce(b.GCSVerbal,5)
                                + coalesce(b.GCSEyes,4)
                            -- otherwise, add up score normally, imputing previous value if none available at current time
                            else
                                  coalesce(b.GCSMotor,coalesce(b2.GCSMotor,6))
                                + coalesce(b.GCSVerbal,coalesce(b2.GCSVerbal,5))
                                + coalesce(b.GCSEyes,coalesce(b2.GCSEyes,4))
                            end) as GCS_min
                      
                        from base b
                        -- join to itself within 6 hours to get previous value
                        left join base b2
                          on b.ICUSTAY_ID = b2.ICUSTAY_ID
                          and b.rn = b2.rn+1
                          and b2.charttime > b.charttime - interval '6' hour
                        group by b.icustay_id
                      ),
            scorecomp AS
                      (
                        select ie.icustay_id
                          , min(v.SysBP_Min) as SysBP_Min
                          , max(v.RespRate_max) as RespRate_max
                          , min(gcs.GCS_min) as GCS_min
                          , max(case when ve.icustay_id is not null then 1 else 0 end) as vent
                          , max(case when va.icustay_id is not null then 1 else 0 end) as vaso
                      
                        from `physionet-data.mimiciii_clinical.icustays` ie
                        left join vitals v
                          on ie.icustay_id = v.icustay_id
                        left join gcs gcs
                          on ie.icustay_id = gcs.icustay_id
                        -- extend the starttime backward 6 hours
                        -- thus, a patient is treated as ventilated if the vent started/ended at most 6 hours after admission
                        -- this also lets us include patients ventilated before ICU admission
                        left join `physionet-data.mimiciii_derived.ventdurations` ve
                          on ie.icustay_id = ve.icustay_id
                          and ie.intime between ve.starttime - interval '6' hour and ve.endtime + interval '6' hour
                        -- similarly, we look for vasopressor usage on admission
                        left join `physionet-data.mimiciii_derived.vasopressin_durations` va
                          on ie.icustay_id = va.icustay_id
                          and ie.intime between va.starttime - interval '6' hour and va.endtime + interval '6' hour
                        group by ie.icustay_id
                      ),
                      
            scorecalc as
                      (
                        -- Calculate the final score
                        -- note that if the underlying data is missing, the component is null
                        -- eventually these are treated as 0 (normal), but knowing when data is missing is useful for debugging
                        select s.*
                        -- qSOFA components factoring in ventilation/vasopressor usage
                        , case
                            when vaso = 1 then 1
                            when SysBP_Min is null then null
                            when SysBP_Min   <= 100 then 1
                            else 0 end
                          as SysBP_score
                        , case
                            when GCS_min is null then null
                            when GCS_min   <= 13 then 1
                            else 0 end
                          as GCS_score
                        , case
                            when vent = 1 then 1
                            when RespRate_max is null then null
                            when RespRate_max   >= 22 then 1
                            else 0 end
                          as RespRate_score
                      
                          -- qSOFA components if we do not factor in ventilation/vasopressor usage
                          , case
                              when SysBP_Min is null then null
                              when SysBP_Min   <= 100 then 1
                              else 0 end
                            as SysBP_score_norx
                          , case
                              when GCS_min is null then null
                              when GCS_min   <= 13 then 1
                              else 0 end
                            as GCS_score_norx
                          , case
                              when RespRate_max is null then null
                              when RespRate_max   >= 22 then 1
                              else 0 end
                            as RespRate_score_norx
                      
                        from scorecomp s
                      ),
            
            QSOFA_admit AS (
                select s.*
                  , coalesce(SysBP_score,0)
                    + coalesce(GCS_score,0)
                    + coalesce(RespRate_score,0)
                    as qSOFA
                  , coalesce(SysBP_score_norx,0)
                    + coalesce(GCS_score_norx,0)
                    + coalesce(RespRate_score_norx,0)
                    as qSOFA_no_rx
                from scorecalc s
                order by icustay_id
            ),
            
            co_dx AS (
              SELECT hadm_id
              	-- sepsis codes
              	, MAX(
                  	CASE
                  		WHEN icd9_code = '99592' THEN 1
                    ELSE 0 END
                  ) AS severe_sepsis
              	, MAX(
                  	CASE
                  		WHEN icd9_code = '78552' THEN 1
                    ELSE 0 END
                  ) AS septic_shock
                from `physionet-data.mimiciii_clinical.diagnoses_icd`
                GROUP BY hadm_id
            )
            
            select co.icustay_id, co.hadm_id
              , co.excluded
              -- suspicion POE
              , co.suspected_infection_time_poe
              , co.age
              , co.gender
              , case when co.gender = 'M' then 1 else 0 end as is_male
              , co.ethnicity
          
              -- ethnicity flags
              , case when co.ethnicity in
              (
                   'WHITE' --  40996
                 , 'WHITE - RUSSIAN' --    164
                 , 'WHITE - OTHER EUROPEAN' --     81
                 , 'WHITE - BRAZILIAN' --     59
                 , 'WHITE - EASTERN EUROPEAN' --     25
              ) then 1 else 0 end as race_white
              , case when co.ethnicity in
              (
                    'BLACK/AFRICAN AMERICAN' --   5440
                  , 'BLACK/CAPE VERDEAN' --    200
                  , 'BLACK/HAITIAN' --    101
                  , 'BLACK/AFRICAN' --     44
                  , 'CARIBBEAN ISLAND' --      9
              ) then 1 else 0 end as race_black
              , case when co.ethnicity in
              (
                'HISPANIC OR LATINO' --   1696
              , 'HISPANIC/LATINO - PUERTO RICAN' --    232
              , 'HISPANIC/LATINO - DOMINICAN' --     78
              , 'HISPANIC/LATINO - GUATEMALAN' --     40
              , 'HISPANIC/LATINO - CUBAN' --     24
              , 'HISPANIC/LATINO - SALVADORAN' --     19
              , 'HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)' --     13
              , 'HISPANIC/LATINO - MEXICAN' --     13
              , 'HISPANIC/LATINO - COLOMBIAN' --      9
              , 'HISPANIC/LATINO - HONDURAN' --      4
            ) then 1 else 0 end as race_hispanic
            
            , case when co.ethnicity in (
                'ASIAN' --   1509
              , 'ASIAN - CHINESE' --    277
              , 'ASIAN - ASIAN INDIAN' --     85
              , 'ASIAN - VIETNAMESE' --     53
              , 'ASIAN - FILIPINO' --     25
              , 'ASIAN - CAMBODIAN' --     17
              , 'ASIAN - OTHER' --     17
              , 'ASIAN - KOREAN' --     13
              , 'ASIAN - JAPANESE' --      7
              , 'ASIAN - THAI' --      4
            ) then 1 else 0 end as race_asian
            , case when co.ethnicity not in
            (
                'WHITE' --  40996
              , 'WHITE - RUSSIAN' --    164
              , 'WHITE - OTHER EUROPEAN' --     81
              , 'WHITE - BRAZILIAN' --     59
              , 'WHITE - EASTERN EUROPEAN' --     25
              , 'BLACK/AFRICAN AMERICAN' --   5440
              , 'BLACK/CAPE VERDEAN' --    200
              , 'BLACK/HAITIAN' --    101
              , 'BLACK/AFRICAN' --     44
              , 'CARIBBEAN ISLAND' --      9
              , 'HISPANIC OR LATINO' --   1696
              , 'HISPANIC/LATINO - PUERTO RICAN' --    232
              , 'HISPANIC/LATINO - DOMINICAN' --     78
              , 'HISPANIC/LATINO - GUATEMALAN' --     40
              , 'HISPANIC/LATINO - CUBAN' --     24
              , 'HISPANIC/LATINO - SALVADORAN' --     19
              , 'HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)' --     13
              , 'HISPANIC/LATINO - MEXICAN' --     13
              , 'HISPANIC/LATINO - COLOMBIAN' --      9
              , 'HISPANIC/LATINO - HONDURAN' --      4
              , 'ASIAN' --   1509
              , 'ASIAN - CHINESE' --    277
              , 'ASIAN - ASIAN INDIAN' --     85
              , 'ASIAN - VIETNAMESE' --     53
              , 'ASIAN - FILIPINO' --     25
              , 'ASIAN - CAMBODIAN' --     17
              , 'ASIAN - OTHER' --     17
              , 'ASIAN - KOREAN' --     13
              , 'ASIAN - JAPANESE' --      7
              , 'ASIAN - THAI' --      4
            ) then 1 else 0 end as race_other
              -- other races
              -- , 'UNKNOWN/NOT SPECIFIED' --   4523
              -- , 'OTHER' --   1512
              -- , 'UNABLE TO OBTAIN' --    814
              -- , 'PATIENT DECLINED TO ANSWER' --    559
              -- , 'MULTI RACE ETHNICITY' --    130
              -- , 'PORTUGUESE' --     61
              -- , 'AMERICAN INDIAN/ALASKA NATIVE' --     51
              -- , 'MIDDLE EASTERN' --     43
              -- , 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER' --     18
              -- , 'SOUTH AMERICAN' --      8
              -- , 'AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE' --      3
          
              , eli.metastatic_cancer
              , case when eli.diabetes_uncomplicated = 1
                      or eli.diabetes_complicated = 1
                          then 1
                  else 0 end as diabetes
          
              , ht.Height
              , wt.Weight
              , wt.Weight / (ht.Height/100*ht.Height/100) as bmi
          
              -- service type on hospital admission
              , case when co.first_service IN ('MED', 'CMED') THEN co.first_service else 'Other'
              end as first_service 
          
              -- outcomes
              , adm.HOSPITAL_EXPIRE_FLAG
              , case when pat.dod <= adm.admittime + interval '30' day then 1 else 0 end
                  as THIRTYDAY_EXPIRE_FLAG
              , ie.los as icu_los
              , DATETIME_DIFF(adm.dischtime, adm.admittime, DAY) as hosp_los
          
              -- in-hospital mortality score (van Walraven et al.)
              ,   CONGESTIVE_HEART_FAILURE    *(4)    + CARDIAC_ARRHYTHMIAS   *(4) +
                  VALVULAR_DISEASE            *(-3)   + PULMONARY_CIRCULATION *(0) +
                  PERIPHERAL_VASCULAR         *(0)    + HYPERTENSION*(-1) + PARALYSIS*(0) +
                  OTHER_NEUROLOGICAL          *(7)    + CHRONIC_PULMONARY*(0) +
                  DIABETES_UNCOMPLICATED      *(-1)   + DIABETES_COMPLICATED*(-4) +
                  HYPOTHYROIDISM              *(0)    + RENAL_FAILURE*(3) + LIVER_DISEASE*(4) +
                  PEPTIC_ULCER                *(-9)   + AIDS*(0) + LYMPHOMA*(7) +
                  METASTATIC_CANCER           *(9)    + SOLID_TUMOR*(0) + RHEUMATOID_ARTHRITIS*(0) +
                  COAGULOPATHY                *(3)    + OBESITY*(-5) +
                  WEIGHT_LOSS                 *(4)    + FLUID_ELECTROLYTE         *(6) +
                  BLOOD_LOSS_ANEMIA           *(0)    + DEFICIENCY_ANEMIAS      *(-4) +
                  ALCOHOL_ABUSE               *(0)    + DRUG_ABUSE*(-6) +
                  PSYCHOSES                   *(-5)   + DEPRESSION*(-8)
              as elixhauser_hospital
          
              , case when vent.starttime is not null then 1 else 0 end as vent
              
              ,vit.HeartRate_Min
              ,vit.HeartRate_Max
              ,vit.SysBP_Min
              ,vit.DiasBP_Mean
              ,vit.MeanBP_Min
              ,vit.RespRate_Mean
              ,vit.TempC_Min
              ,vit.TempC_Max
              ,vit.SpO2_Mean
              ,lab.aniongap_min
              ,lab.aniongap_max
              ,lab.creatinine_min
              ,lab.chloride_min
              ,lab.hemoglobin_min
              ,lab.hemoglobin_max
              ,lab.lactate_min
              ,lab.platelet_max
              ,lab.potassium_min
              ,lab.inr_max
              ,lab.sodium_min
              ,lab.sodium_max
              ,lab.bun_min
              ,lab.bun_max
              ,lab.wbc_min
              ,lab.wbc_max
              ,u.urineoutput
          
              , so.sofa as sofa
              , lo.lods as lods
              , si.sirs as sirs
              , qs.qsofa as qsofa
          
          from sepsis3_cohort co
          inner join `physionet-data.mimiciii_clinical.icustays` ie
            on co.icustay_id = ie.icustay_id
          inner join `physionet-data.mimiciii_clinical.admissions` adm
            on ie.hadm_id = adm.hadm_id
          inner join `physionet-data.mimiciii_clinical.patients` pat
            on ie.subject_id = pat.subject_id
          left join `physionet-data.mimiciii_derived.elixhauser_ahrq_v37` eli
            on ie.hadm_id = eli.hadm_id
          left join `physionet-data.mimiciii_derived.heightfirstday` ht
            on ie.icustay_id = ht.icustay_id
          left join `physionet-data.mimiciii_derived.weightfirstday` wt
            on ie.icustay_id = wt.icustay_id
          left join `physionet-data.mimiciii_derived.blood_culture_icu_admit` bc
            on ie.icustay_id = bc.icustay_id
          left join
            ( select icustay_id, min(starttime) as starttime
              from `physionet-data.mimiciii_derived.ventdurations`
              group by icustay_id
            ) vent
            on co.icustay_id = vent.icustay_id
            and vent.starttime >= co.intime - interval '4' hour
            and vent.starttime <= co.intime + interval '1' day
          left join `physionet-data.mimiciii_derived.sofa` so
            on co.icustay_id = so.icustay_id
          left join `physionet-data.mimiciii_derived.sirs` si
            on co.icustay_id = si.icustay_id
          left join `physionet-data.mimiciii_derived.lods` lo
            on co.icustay_id = lo.icustay_id
          left join `physionet-data.mimiciii_derived.qsofa` qs
            on co.icustay_id = qs.icustay_id
          left join `physionet-data.mimiciii_derived.vitals_first_day` vit
            on co.icustay_id = vit.icustay_id
          left join `physionet-data.mimiciii_derived.labs_first_day` lab
            on co.icustay_id = lab.icustay_id
          left join `physionet-data.mimiciii_derived.urine_output_first_day` u
            on co.icustay_id = u.icustay_id
          -- left join `physionet-data.mimiciii_derived.mlods` ml
          --   on co.icustay_id = ml.icustay_id
          -- left join QSOFA_admit qsadm
          --   on co.icustay_id = qsadm.icustay_id
          -- left join SIRS_admit siadm
          --   on co.icustay_id = siadm.icustay_id
          order by co.icustay_id;"

co <- dbGetQuery(con, sepsis3_alistairewj)
sepsis3 <- subset(co, excluded == 0)
sepsis3 <- sepsis3[!is.na(sepsis3$suspected_infection_time_poe) & sepsis3$sofa >= 2, ]