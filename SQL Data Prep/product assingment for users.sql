-- Selecting drugs assigned to teams as their pharmacogological class and active ingredients in both DPV and DMEPA ---
use MEET;
select division, count(*) from product
where division in ('dpv','dmepa')
group by division;

select * from product;
select count(distinct([Pharm_Class (PC)])) from product


-------- For DPV, view how many products, PAI's,  are assigned to each user 
select [user name] , count(distinct[Pharm_Class (PC)]), count (distinct([Product_Active_Ingredient(PAI)])) PAI  from product
where division = 'dpv'
group by [user name]
order by [user name],c;

select [user name] ,  count (distinct([Pharm_Class (PC)])) PAI from product
where division = 'dpv'
group by [user name] 
order by PAI;


-------- For DMEPA, view how many products, PAI's,  are assigned to each user 
select ([Pharm_Class (PC)]), count(distinct([Product_Active_Ingredient(PAI)])) PAI from product
where division = 'dmepa'
group by ([Pharm_Class (PC)])
order by PAI asc;

select [user name] , ([Pharm_Class (PC)])   , count([Product_Active_Ingredient(PAI)]) PAI from product
where division = 'dmepa'
group by ([Pharm_Class (PC)]) , [user name] 
order by PAI asc;

select [user name] , count(distinct[Pharm_Class (PC)]), count (distinct([Product_Active_Ingredient(PAI)])) PAI  from product
where division = 'dmepa'
group by [user name]
order by [user name],PAI;