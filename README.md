# :pushpin: A tool for analysis and prediction of the MIMIC-III data :computer:

### Index

1. [Explanation](#explanation):woman_teacher:
2. [Resumo da Dissertação](#resumo-da-dissertação):bookmark_tabs: 
3. [Dissertation Abstract](#dissertation-abstract):bookmark_tabs: 
4. [Files Explanation](#files-explanation)📂
5. [Requirements](#requirements)🗃️
6. [How to use](#how-to-use)❓
7. [Notes about the work](#notes)📔


---------------------------------------------------------------------------------

# Explanation
A tool already existed, which is a graphical interface in R that collects the data relating to the patients in this database, relates the various tables in it, and presents the relevant information in a simple way, but without neglecting functionality. It contains graphics, tables and text generated in real time, so that the user can find out many details about the MIMIC-III database without needing to know any kind of programming language.
Our tool is therefore a complement to the existing one. A new menu has been added to it, in which the user can analyze the data in different ways, in relation to a disease, select features for the models, train and compare models and then use them to make predictions. At the moment, and as the case study is sepsis, there are only two target variables, mortality from sepsis and 30-day mortality from sepsis.
<br>

# Resumo da dissertação

O conjunto de dados MIMIC-III, uma rica fonte de informações clínicas provenientes de unidades de cuidados intensivos, desempenha um papel crucial na análise e previsão da sépsis, uma condição potencialmente fatal com elevadas taxas de mortalidade. Este estudo concentra seus esforços na exploração dos dados com o objetivo de entender mais profundamente os fatores de risco subjacentes, a progressão da doença e os desfechos associados a essa condição crítica. Adicionando uma interface à base de dados MIMIC-III, tornamos possível o estudo dos pacientes que dão entrada na UCI, permitindo a visualização dos dados dos pacientes e a construção de modelos preditivos. Para um estudo de caso, utilizamos uma condição potencialmente fatal em pacientes internados, com elevadas taxas de mortalidade.

O desenvolvimento de uma ferramenta robusta para analisar e prever a sépsis a partir desses dados é de extrema importância para possibilitar a deteção precoce, intervenções oportunas e tratamentos personalizados. Essa ferramenta tem o potencial de auxiliar os médicos na identificação imediata desta doença, bem como na avaliação de riscos e na alocação eficaz de recursos, o que pode resultar em melhores desfechos para os pacientes.

Utilizando algoritmos avançados de estatística e aprendizagem de máquina, este estudo identificou padrões ocultos, correlações e relações preditivas específicas da sépsis nos dados da MIMIC-III.

As principais contribuições deste trabalho incluem o desenvolvimento de uma ferramenta geral para análise e previsão de dados da MIMIC-III, com a sépsis atuando como um estudo de caso. Utilizando a mortalidade por sépsis e a mortalidade em 30 dias de pacientes com sépsis como pontos focais, a ferramenta permite a coleta de dados dos pacientes, o treinamento de modelos, a comparação entre eles e a avaliação de seu desempenho. O uso do estudo da sépsis serve como um exemplo prático do uso da ferramenta em um cenário clínico específico, destacando sua aplicabilidade e utilidade mais amplas na análise e previsão de doenças críticas.

Para atingir esses objetivos, foram avaliados vários classificadores com estratégias específicas para cada domínio. Com base nos resultados obtidos, o XGBoost destacou-se como uma escolha eficaz para prever tanto a mortalidade em 30 dias por sépsis quanto a mortalidade por sépsis, utilizando um conjunto fixo de variáveis. Demonstrou um desempenho consistente e sólido em ambas as tarefas, sugerindo que é um modelo robusto e adequado para essa aplicação.

Em resumo, os resultados deste estudo não apenas contribuem para a compreensão da sépsis, mas também estabelecem uma base sólida para a pesquisa em diversos cenários de cuidados intensivos e a exploração de uma ampla variedade de desfechos clínicos nos dados da MIMIC-III. Apesar dos resultados satisfatórios, reconhecemos a existência de limitações e a necessidade de futuras melhorias.

# Dissertation Abstract

The MIMIC-III dataset, a rich source of clinical information from intensive care units, plays a crucial role in the analysis and prediction of sepsis, a potentially fatal condition with high mortality rates. This study focuses its efforts on exploring the data to gain a deeper understanding of underlying risk factors, disease progression, and outcomes associated with this critical condition. By adding an interface to the MIMIC-III database, we have made it possible to study patients admitted to the ICU, allowing for patient data visualization and the construction of predictive models. For a case study, we used a potentially fatal condition in hospitalized patients with high mortality rates.

The development of a robust tool to analyze and predict sepsis from this data is of utmost importance to enable early detection, timely interventions, and personalized treatments. This tool has the potential to assist physicians in the immediate identification of this disease, as well as risk assessment and effective resource allocation, which can result in better outcomes for patients.

Using advanced statistical and machine learning algorithms, this study identified hidden patterns, correlations, and specific predictive relationships of sepsis in MIMIC-III data.

The main contributions of this work include the development of a general tool for the analysis and prediction of MIMIC-III data, with sepsis serving as a case study. Focusing on sepsis mortality and 30-day mortality in sepsis patients, the tool enables data collection, model training, comparison, and performance evaluation. The use of sepsis as a case study serves as a practical example of the tool's application in a specific clinical setting, highlighting its broader applicability and utility in the analysis and prediction of critical diseases.

To achieve these objectives, various classifiers were evaluated with domain-specific strategies. Based on the results obtained, XGBoost emerged as an effective choice for predicting both 30-day sepsis mortality and sepsis mortality using a fixed set of variables. It demonstrated consistent and robust performance in both tasks, suggesting that it is a reliable and suitable model for this application.

In summary, the results of this study not only contribute to the understanding of sepsis but also establish a solid foundation for research in various intensive care settings and the exploration of a wide range of clinical outcomes in MIMIC-III data. Despite satisfactory results, we acknowledge the existence of limitations and the need for future improvements.

# Files Explanation:

- **dashboard_beta.R** - main file with the complete tool, which contains the code described in the dissertation.
- **src** foder:
    <ul>
      <li> sepsis.R - file with the data for the sepsis-3 definition. </li>
      <li> dashboard_predictions.R - file with just the <b>menu Predictions</b> from <b>dashboard_beta.R</b> and without the <b>help button</b>, used for tests, but working. </li>
    </ul>

# Requirements:
- R version 4.1.2
- RStudio version 2022.02.3
- Only credentialed users who sign the DUA can access the MIMIC-III data, so it's required the training CITI Data or Specimens Only Research available [here](https://physionet.org/content/mimiciii/1.4/).
- Acesso ao BigQuery, com tutorial de uso disponível [here](https://mimic.mit.edu/docs/iii/tutorials/intro-to-mimic-iii-bq/)

# How to use
Once you have all the requirements, you just need to click on the following button, in the top right corner of the RStudio main screen:

![runApp](https://github.com/CristianaMorais/A-tool-for-analysis-and-prediction-of-the-MIMIC-III-data/assets/20134178/e034eeb4-97d8-4dbf-92ff-6804d84d88d1)

If you have any questions while using the application, just click on the help button in the top right corner of the application:

![helpButton](https://github.com/CristianaMorais/A-tool-for-analysis-and-prediction-of-the-MIMIC-III-data/assets/20134178/66dae2a1-4846-4eb6-b855-3f2bbd985351)

# Notes
> :link: This is the code repository associated with the masters dissertation, it's available [here](add).

> :link: The original app, which is the basis of this tool, it's available [here](https://github.com/nmot97/An-interface-for-exploratory-analasys-of-the-MIMIC-III). 
