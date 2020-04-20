# Gov_1006_final_project

### Description
This is the final project for our Gov 1006 class. The task is to reproduce an academic paper, leveraging  the data and the code that was published with it. I have decided to focus on the research paper: "How Chinese Officials Use the Internet to Construct their Public Image" by Jennifer Pany, published in June 15, 2017.


### Abstract

Pan (2017) shows that the emphasis on Chinese local government websites on either the competence or benevolence of county executives depends on where they are in the political tenure cycle. I was largely able to replicate these results. My extension confirms that this is the most likely explanation for the observed effect by comparing the statistical explanatory power of alternative models (e.g. cultural differences among regions, gender differences, etc.) through the leave one out method. In addition I validate the geographical randomness of the sample through simulations of repeated sampling and the construction of confidence intervals. They corraborate the findings of the paper by confirming the geographic randomness of the sample.


### Discussion of project organization
The code is organized by the sections of the publication paper. While the code does not create the graphs outline in the paper, it creates the data necessary to create the respective graphs. These include:

1. Section 3.1: Website Content
   Creates a table by provinces on availability of websites  
   
2. Section 4.1: Topics
  Create word clouds on topics per OGI cluster

3. Section 5.1: Measuring Tenure
  Creating a proximity to leaving office tables based on number of counties in which a certain proximity applied
  
4. Section 5.2: Descriptive Results
  Figure to visualize proportion of web pages with content focused on competence 
  Figure to visualize proportion of web pages with content focused on benevolence
  

5. Section 5.3: Predictive Inference
  Statistical summary of the linear regression model that regresses various indicators on mentions of competence or benevolence on websites

The raw-data is organized into:
- R code
- CSV of county wesbites
- CSV of 100 randomly sampled county websites
- CSV of prediction model
- CSV of competence prediction model
- CSV of benevolence prediction model
- CSV of  topics
- CSV of wordvlouds
- PDF of plots


