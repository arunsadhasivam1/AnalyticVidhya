

train= read.csv("train.csv",header=TRUE)
intern= read.csv("Internship.csv",header=TRUE)
student= read.csv("Student.csv",header=TRUE)
names(student)

library(data.table)



#head(train,10) 
library(plyr)

traindata<-join(train, intern,by='Internship_ID',
                type = "left" , match = "first")


studentdata<-join(traindata, student,by='Student_ID',type = "left", match = "first" )
areaofInterest<-c('UI',
                  'Marketing',
                  'Media',
                  'Social',
                  'Design',
                  'Web',
                  'Development',
                  'Business',
                  'Research',
                  'Writing',
                  'Plan',
                  'Creative',
                  'Process',
                  'Database',
                  'Strategy',
                  'Designing',
                  'Analysis',
                  'Facebook',
                  'Communication',
                  'Rest',
                  'Android',
                  'Presentation',
                  'MediaMarketing',
                  'Twitter',
                  'SocialMediaMarketing',
                  'Operations',
                  'Java',
                  'Quality',
                  'HTML',
                  'Blogs',
                  'DigitalMarketing',
                  'PHP',
                  'MarketResearch',
                  'Recruitment',
                  'Testing',
                  'CSS',
                  'Planning',
                  'API',
                  'Editing',
                  'ContentWriting',
                  'Innovative',
                  'LeadGeneration',
                  'MobileApp',
                  'SQL',
                  'Accounts',
                  'Reporting',
                  'JavaScript',
                  'Documentation',
                  'iOS',
                  'Branding',
                  'ACTING',
                  'Analytics',
                  'Initiative',
                  'Advertising',
                  'ColdCalling',
                  'Sourcing',
                  'ERP',
                  'NGO',
                  'Environment',
                  'Networking',
                  'Production',
                  'MySQL',
                  'ISO',
                  'MarketingStrategy',
                  'Survey',
                  'Visio',
                  'AppDevelopment',
                  'FrontEnd',
                  'webdevelopment',
                  'Integration',
                  'HTML5',
                  'jQuery',
                  'Server',
                  'Coding',
                  'MBA',
                  'ContentCreation',
                  'Reading',
                  'B2B',
                  'ContentDevelopment',
                  'Storm',
                  'E-commerce',
                  'Databases',
                  'Programming',
                  'Wordpress',
                  'Outreach',
                  'NABL',
                  'WebDesign',
                  'Architecture',
                  'WebApplication',
                  'Adobe',
                  'Scala',
                  'UI/UX',
                  'Python',
                  'Mac',
                  'Retail',
                  'DigitalMedia',
                  'ProductDevelopment',
                  'DataCollection',
                  'Algorithm',
                  'LESS',
                  'EmailMarketing',
                  'Screening',
                  'Bootstrap',
                  'Finance',
                  'ContentMarketing',
                  'CommunicationSkil',
                  'Hiring',
                  'Negotiation',
                  'Administration',
                  'CommunicationSkills',
                  'CSS3',
                  'Infographic',
                  'Youtube',
                  'CRM',
                  'CAD',
                  'Infographics',
                  'Access',
                  'Editorial',
                  'ARM',
                  'AJAX',
                  '.NET',
                  'Co-ordination',
                  'Ownership',
                  'Algorithms',
                  'Node',
                  'Drafting',
                  'Blogging',
                  'Animation',
                  'Teaching',
                  'Blogger',
                  'RelationshipManagement',
                  '3d',
                  'HTTP',
                  'PressRelease',
                  'Accounting',
                  'AndroidAppDevelopment',
                  'AdobePhotoshop',
                  'Photography',
                  'SoftwareDevelopment',
                  'SocialNetworking',
                  'AngularJS',
                  'AWS',
                  'SecondaryResearch',
                  'Recruiting',
                  'ClientServicing',
                  'Leadership',
                  'ContentWriter',
                  'WebServices',
                  'Payroll',
                  'Prospecting',
                  'GraphicDesigning',
                  'Proofreading',
                  'DataEntry',
                  'Flex',
                  'Creativity',
                  'DataManagement',
                  'Convincing',
                  'GATE',
                  'SocialMediaManagement',
                  'MachineLearning',
                  'ClientRelations',
                  'WebApplications',
                  'XML',
                  'MVC',
                  'HTML/CSS',
                  'Google+',
                  'Typing',
                  'Sketch',
                  'UIDesign',
                  'VisualDesign',
                  'CreativeWriting',
                  'GraphicDesigner',
                  'ProductDesign',
                  'PERL',
                  'Hindi',
                  'Chef',
                  'SalesProcess',
                  'ASP.NET',
                  'Django',
                  'PublicRelations',
                  'CMS',
                  'VendorManagement',
                  'ContentStrategy',
                  'ClientRelationship',
                  'CreativeDesign',
                  'C#',
                  'JSON',
                  'Linux',
                  'ClientInteraction',
                  'Manufacturing',
                  'CustomerRelationshipManagement',
                  'RecruitmentProcess',
                  'BusinessRelation',
                  'TalentAcquisition',
                  'CorelDRAW',
                  'BigData',
                  'MaterialDesign',
                  'MarketAnalysis',
                  'AdobeIllustrator',
                  'RESTAPI',
                  'Tally',
                  'Electronics',
                  'Bee',
                  'C++',
                  'OnlineResearch',
                  'Mockups',
                  'FrontEndDevelopment',
                  'Gif',
                  'ProductManagement',
                  'MongoDB',
                  'PrimaryResearch',
                  'Healthcare',
                  'DataAnalytics',
                  'GoogleAnalytics',
                  'CorePHP',
                  'B2BSales',
                  'SocialMediaTools',
                  'Node.js',
                  'Ruby',
                  'Drawing',
                  'BrandPromotion',
                  'Mechanical',
                  'Automobile',
                  'Lifestyle',
                  'WritingBlogs',
                  'CodeIgniter',
                  'WritingSkills',
                  'SQLServer',
                  'LogoDesign',
                  'ProjectManagement',
                  'APIIntegration',
                  'ClientCommunication',
                  'GrowthHacking',
                  'InteriorDesign',
                  'Personality',
                  'SAP',
                  'Scripting',
                  'AndroidApplicationDevelopment',
                  'EventManagement',
                  'BlogWriting',
                  'Statistics',
                  'Typography'
)



to.string <- function(x) {
  string <- x[1]
  for(i in 2:length(x)) {
    string <- paste(string, x[i], sep=",")
  }
  return(string)
}




totRow <-nrow(intern)
totCol<-length(areaofInterest) 
Internship_ID <-c()
AreaOfinterest<-c()
AreaInterestCount<-c()
row<-1
interest<-''
while(row<=totRow )  {
  col<-1  
  totcount<-0
  while(col<=totCol){
    colname<-areaofInterest[col]
    coldata<-intern[row, colname]
    if( !is.null(coldata) ){
      if(coldata==1){
        interest<- paste(colname,interest , sep=",",collapse = '')
        totcount<-totcount+1
      } 
    } 
    
    col<- col+1
  }
  # print(intern[row,1])
  Internship_ID[row]=intern[row,1]
  result = substr(interest, 1, nchar(interest)-1)
  AreaOfinterest[row]=result
  #print(totcount)
  AreaInterestCount[row]=totcount
  interest<-''
  row<- row+1 
}

AreaofInterestdata<-data.table(Internship_ID,AreaOfinterest,AreaInterestCount)
dtinterest <-as.data.frame(AreaofInterestdata)

result<-join(studentdata,dtinterest ,by='Internship_ID',
             type = "left"  ,match = "first")



combined <- result[c('Student_ID','Internship_ID'
                     ,'Internship_Profile','Degree','hometown','Is_Part_Time'
                     ,'Earliest_Start_Date','Expected_Stipend','Minimum_Duration','Preferred_location','Is_Part_Time'
                     ,'Stream',	'Current_year' 
                     ,'Earliest_Start_Date'
                     ,'Profile','Location'
                     ,'Start.Date','End.Date'
                     ,'AreaOfinterest','Experience_Type'
                     ,'AreaInterestCount'
                     ,'Year_of_graduation','Performance_PG','Performance_UG' 
                     ,'Performance_12th','Performance_10th','Experience_Type'
                     ,'Expected_Stipend','Is_Shortlisted','No_of_openings' )]


combined$No_of_openings<-as.factor(combined$No_of_openings)

combined[is.na(combined$AreaInterestCount),'AreaInterestCount']<-0
combined$AreaOfinterest<-as.factor(combined$AreaOfinterest)



combined$Degree<-as.factor(combined$Degree)
combined$Performance_PG<-as.factor(combined$Performance_PG)
combined$Performance_PG<-as.factor(combined$Performance_PG)
combined$Performance_UG<-as.factor(combined$Performance_UG)
combined$Performance_12th<-as.factor(combined$Performance_12th)
combined$Performance_10th<-as.factor(combined$Performance_10th)
combined$Experience_Type<-as.factor(combined$Experience_Type)
combined$Stream<-as.factor(combined$Stream)
combined$Is_Shortlisted<-as.factor(combined$Is_Shortlisted)


combined[is.na(combined$Internship_ID),'Internship_ID']<-0
testdata= read.csv("test.csv",header=TRUE)
testsubmit<-join(testdata, combined,by='Student_ID',
                 type = "left" ,match="first"  )

#write.csv( file ="combined.csv" ,testsubmit)
########################### validation and format on test data
testsubmit[is.na(testsubmit$AreaInterestCount),'AreaInterestCount']<-0  
testsubmit$Is_Shortlisted <-as.integer( testsubmit$Is_Shortlisted)
testsubmit[is.na(testsubmit$Is_Shortlisted),'Is_Shortlisted']<-0 


#########################################filter######################
testsubmit[grepl('Accounts',testsubmit$AreaOfinterest)=='TRUE'
           & grepl('Rest',testsubmit$AreaOfinterest)=='TRUE'
           & grepl('Development',testsubmit$AreaOfinterest)=='TRUE'
           ,'Is_Shortlisted']<-0

testsubmit[grepl('Editorial',testsubmit$AreaOfinterest)=='TRUE' & 
             !grepl('Content',testsubmit$AreaOfinterest)=='TRUE'&
             !grepl('Writing',testsubmit$AreaOfinterest)=='TRUE'
           ,'Is_Shortlisted']<-0

#facebook
testsubmit[grepl('Facebook,Creative,Social,Media,Marketing',testsubmit$AreaOfinterest)=='TRUE' 
           ,'Is_Shortlisted']<-0

testsubmit[grepl('Facebook,Creative,Web,Design',testsubmit$AreaOfinterest)=='TRUE' 
           ,'Is_Shortlisted']<-0

testsubmit[grepl('Facebook,Designing,Web,Design',testsubmit$AreaOfinterest)=='TRUE' 
           ,'Is_Shortlisted']<-0
testsubmit[grepl('Facebook,Research,Marketing',testsubmit$AreaOfinterest)=='TRUE' 
           ,'Is_Shortlisted']<-0

testsubmit[grepl('Facebook,Social,Media,Marketing',testsubmit$AreaOfinterest)=='TRUE' 
           ,'Is_Shortlisted']<-0

#flex
testsubmit[grepl('Youtube',testsubmit$AreaOfinterest)=='TRUE' 
           ,'Is_Shortlisted']<-0
#Hindi
testsubmit[grepl('Hindi',testsubmit$AreaOfinterest)=='TRUE' 
           & !grepl('Arts',testsubmit$Stream ) =='TRUE'
           & !grepl('Management',testsubmit$Stream ) =='TRUE'
           ,'Is_Shortlisted']<-0



testsubmit[grepl('Typing',testsubmit$AreaOfinterest)=='TRUE'
           & !grepl('Testing',testsubmit$AreaOfinterest)=='TRUE'
           & !grepl('Designing',testsubmit$AreaOfinterest)=='TRUE'
           & !grepl('Creative',testsubmit$AreaOfinterest)=='TRUE' 
           & !grepl('Development',testsubmit$AreaOfinterest)=='TRUE'
           & !grepl('Design',testsubmit$AreaOfinterest)=='TRUE' 
           ,'Is_Shortlisted']<-0

library(mlbench)
library(adabag)
library("ggplot2")

formula <- Is_Shortlisted ~ Internship_Profile+	 Degree+Stream+AreaOfinterest+AreaInterestCount+No_of_openings
bst<- boosting(formula, data=combined, boos=TRUE,mfinal=4.95)
importanceplot(bst)
pred<- predict.bagging(bst, newdata=testsubmit)
pred


###########################################shortlisted
testsubmit[grepl('Social Media Marketing And Content Writing',testsubmit$Internship_Profile)=='TRUE' ,'Is_Shortlisted']<-1
#testsubmit[grepl('Event Co-ordinator',testsubmit$Internship_Profile)=='TRUE' ,'Is_Shortlisted']<-1
#############################submit result afer formatting. 

write.csv(file = "testresult.csv",c(testsubmit['Internship_ID'],testsubmit['Student_ID'],testsubmit['Is_Shortlisted']), row.names=F)
