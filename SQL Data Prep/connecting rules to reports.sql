-- Joining generated rules for a quarter to their respect raw FAERS reports

use fda_new;

select * into rules_updated from rules14q4;

update rules_updated
set ADRs =  replace(replace(ADRs, '[', ''), ']', '');

update rules_updated
set ADRs = replace(ADRs, ' ', ',');

select * from rules_updated
where ADRs like '% %'

drop table new_table

-- TO SEE HOW MANY ADRs ARE THERE INSIDE EACH adrS COLUMN IN RULES DATA
SELECT adrS, LEN(adrS)- LEN(REPLACE(adrS, ',', '')) + 1   FROM rules_updated
ORDER BY LEN(adrS)- LEN(REPLACE(adrS, ',', '')) + 1 DESC

-- or

-- TO FIND MAX ADRS IN ONE RULE, SO THAT THOSE MANY SEPERATE COLUMNS ARE CREATED WITH CTE
SELECT MAX( LEN(adrS)- LEN(REPLACE(adrS, ',', '')) + 1 )  FROM rules_updated

--create each ADR in ADRs as a separate ADR
;WITH cte ([Rank]
      ,[Score]
      ,[No_of_Drugs]
      ,[Drugname]
	  ,[ADRs]
      ,[Support]
      ,[Confidence]
      ,[Drug1]
      ,[Support1]
      ,[Confidence1]
      ,[Drug2]
      ,[Support2]
      ,[Confidence2]
      ,[drugname_updated], ADR_splitted)
AS
(
SELECT 
       [Rank]
      ,[Score]
      ,[No_of_Drugs]
      ,[Drugname]
	   ,[ADRs]
      ,[Support]
      ,[Confidence]
      ,[Drug1]
      ,[Support1]
      ,[Confidence1]
      ,[Drug2]
      ,[Support2]
      ,[Confidence2]
      ,[drugname_updated],
	      CONVERT(XML,'<ADR><Attribute>' 
        + REPLACE(ADRs,',', '</Attribute><Attribute>') 
        + '</Attribute></ADR>') AS ADR_splitted
FROM rules_updated)
SELECT 
   [Rank]
      ,[Score]
      ,[No_of_Drugs]
      ,[Drugname]
	  ,[ADRs]
      ,[Support]
      ,[Confidence]
      ,[Drug1]
      ,[Support1]
      ,[Confidence1]
      ,[Drug2]
      ,[Support2]
      ,[Confidence2]
      ,[drugname_updated],
    ADR_splitted.value('/ADR[1]/Attribute[1]','varchar(100)') AS ADR1,
    ADR_splitted.value('/ADR[1]/Attribute[2]','varchar(100)') AS ADR2,
    ADR_splitted.value('/ADR[1]/Attribute[3]','varchar(100)') AS ADR3,
    ADR_splitted.value('/ADR[1]/Attribute[4]','varchar(100)') AS ADR4,
	ADR_splitted.value('/ADR[1]/Attribute[5]','varchar(100)') AS ADR5,
	ADR_splitted.value('/ADR[1]/Attribute[6]','varchar(100)') AS ADR6,
	ADR_splitted.value('/ADR[1]/Attribute[7]','varchar(100)') AS ADR7
into new_table
FROM cte


select * from new_table


select * from all_grouped

alter table all_grouped
add drugname_updated  varchar(4000),
sideEffect_updated  varchar(4000)


--- TO REMOVE THE . IN THE DRUGNAMES AND SPACES TO MATCH THE RULES DATA
update all_grouped
set drugname_updated= replace(replace(drugname, ' ', ''),'.','') ,
sideEffect_updated = replace(sideEffect, ' ', '');

-- to remove any extra special character from drugname and ADRs to match the rules data
update all_grouped
set sideEffect_updated = dbo.RemoveSpecialChars (sideEffect_updated), drugname_updated=dbo.RemoveSpecialChars (drugname_updated)  ;

--- TO SELECT THE REPORTS WITH DRUGS INT THE RULES
SELECT e.primaryID, e.drugname_updated,e.sideEffect_updated,   
	   [Rank]
      ,[Score]
      ,[No_of_Drugs]
      ,[ADRs]
	  ,r.[Drugname] as r_drugname
      ,[Support]
      ,[Confidence]
      ,[Drug1]
      ,[Support1]
      ,[Confidence1]
      ,[Drug2]
      ,[Support2]
      ,[Confidence2]
      ,[ADR1]
      ,[ADR2]
      ,[ADR3]
      ,[ADR4]
      ,[ADR5]
	  ,[adr6]
	  ,ADR7
into joined_rules_drugs
FROM  all_grouped e INNER JOIN new_table r
ON e.drugname_updated like CONCAT('%',  drug1 , '%') and e.drugname like CONCAT('%',  drug2 , '%');


drop table joined_rules_drugs
drop table joined_rules_drugs_adrs

-- TO SELECT THE REPORTS WITH adrs IN THE RULES
select * 
into joined_rules_drugs_adrs
from joined_rules_drugs
where sideEffect_updated like  CONCAT('%',  ADR1 , '%') and  sideEffect_updated like  CONCAT('%',  ADR2 , '%') and  sideEffect_updated like  CONCAT('%',  ADR3 , '%') and
sideEffect_updated like  CONCAT('%',  ADR4 , '%') AND sideEffect_updated like  CONCAT('%',  ADR5 , '%') AND
sideEffect_updated like  CONCAT('%',  ADR6 , '%') AND sideEffect_updated like  CONCAT('%',  ADR7 , '%');

--TO MAKE SURE THAT THE SUM OF SUPPORT AND COUNT OF REPORTS MATCH
SELECT COUNT(*) FROM joined_rules_drugs_adrs
SELECT COUNT(*) FROM joined_rules_drugs
--TO MAKE SURE THAT THE SUM OF SUPPORT AND COUNT OF REPORTS MATCH

SELECT SUM(SUPPORT) FROM rules14q4; 


SELECT COUNT(DISTINCT PRIMARYid) FROM joined_rules_drugs_adrs

--- to export as reports data for the viz
SELECT * FROM all_grouped
WHERE PRIMARYID IN (SELECT PRIMARYID FROM joined_rules_drugs_adrs)

---- to group the id's in the rules for the viz
SELECT [Rank]
      ,[Score]
      ,[ADRs] as ADR
      ,r_Drugname 
      ,[Support]
      ,[Confidence]
      ,[Drug1]
      ,[Support1]
      ,[Confidence1]
      ,[Drug2]
      ,[Support2]
      ,[Confidence2]
	  ,fda.dbo.GROUP_CONCAT(distinct(primaryid)) AS id
 from joined_rules_drugs_adrs
 group by
 [Rank]
      ,[Score]
      ,[ADRs]
      ,r_Drugname 
      ,[Support]
      ,[Confidence]
      ,[Drug1]
      ,[Support1]
      ,[Confidence1]
      ,[Drug2]
      ,[Support2]
      ,[Confidence2]



 select count(*) from rules14q4

 ---  some reports with drugs with /100mg }100mg were not joined with rules becoz of string matching
 -- select dbo.RemoveSpecialChars (drugname_updated) from all_grouped 

 update all_grouped
 set drugname_updated = dbo.RemoveSpecialChars (drugname_updated);






