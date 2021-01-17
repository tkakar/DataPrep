use fda;

/*To Insert Demo Data" */
CREATE TABLE  demo14q4 (
primaryid varchar(25) ,
caseid varchar(25),
caseversion varchar (25),
i_f_code varchar(10),
event_dt varchar(25),
mfr_dt varchar(25),
init_fda_dt varchar(25),
fda_dt varchar(25),
rept_cod varchar(25),
mfr_num varchar(500),
mfr_sndr varchar(300),
age varchar(25),
age_cod varchar(25),
gndr_cod varchar(25),
e_sub varchar(25),
wt varchar (25),
wt_cod varchar(25),
rept_dt varchar(25),
to_mfr varchar(100),
occp_cod varchar(4000),
reporter_country varchar(500),
occr_country varchar(25));

/*Drop table demo14q4*/

BULK
INSERT demo14q4
FROM 'C:\Users\tkakar\Desktop\FDA Data\FAERS 2012-2013\Data\demo14q4.txt'
WITH
(
FIRSTROW=2,
FIELDTERMINATOR = '$',
ROWTERMINATOR = '0x0a',
KEEPNULLS  
);


/*select * from demo14q4*/

/*To Insert Drug Data" */

CREATE TABLE  drug14q4 (
primaryid varchar(25) ,
caseid varchar(25),
drug_seq varchar (25),
role_cod varchar(10),
drugname varchar(500),
val_vbm varchar(25),
[route] varchar(25),
dose_vbm varchar(300),
cum_dose_chr varchar(25),
cum_dose_unit varchar(500),
dechal varchar(300),
rechal varchar(25),
lot_nbr varchar(1000),
exp_dt varchar(1000),
nda_num varchar(100),
dose_amt varchar(50),
dose_unit varchar (50),
dose_form varchar(50),
dose_freq varchar(50),
);


BULK
INSERT drug14q4
FROM 'C:\Users\tkakar\Desktop\FDA Data\FAERS 2012-2013\Data\drug14q4.txt'
WITH
(
FIRSTROW=2,
FIELDTERMINATOR = '$',
ROWTERMINATOR = '0x0a',
Keepnulls
);

/*select * from drug14q4*/

/*To Insert Reaction Data" */
CREATE TABLE  reac14q4 (
primaryid varchar(25) ,
caseid varchar(25),
pt varchar(500)
);

BULK
INSERT reac14q4
FROM 'C:\Users\tkakar\Desktop\FDA Data\FAERS 2012-2013\Data\reac14q4.txt'
WITH
(
firstrow=2,
FIELDTERMINATOR = '$',
ROWTERMINATOR = '0x0a',
keepnulls
);

/*select top 5 *  from reac14q4;*/


/*To Insert Reaction Data" */
SELECT demo14q4.primaryid, drug14q4.Drugname, reac14q4.pt
into joined14q4
FROM demo14q4, drug14q4, reac14q4
where demo14q4.rept_cod='EXP' and  demo14q4.primaryid = drug14q4.primaryid and drug14q4.primaryid = reac14q4.primaryid;


update joined14q4
set drugname = '"' + drugname + '"' , pt =  '"' + pt + '"';


/*  Run the Group concat code to install it here--- if not already installed*/
SELECT  primaryid,
        dbo.GROUP_CONCAT(distinct(drugname)) AS drugname, dbo.GROUP_CONCAT(distinct(pt)) As SideEffect
		into grouped14q4
FROM    dbo.joined14q4
GROUP BY primaryid;


/*select * from groupedQ1_2014
select count(*) from joinedQ1_2014
select count(primaryid), count(distinct(primaryid)) from groupedQ1_2014; */

/*Input to the rules algorithm */
select drugname, SideEffect from grouped14q4;