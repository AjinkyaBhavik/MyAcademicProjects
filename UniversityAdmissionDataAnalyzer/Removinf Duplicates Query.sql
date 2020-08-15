

/*


Didnt work as Identity (1,1) doenst work in mysql

ALTER TABLE `users` ADD `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY;

DELETE FROM original_admission_predict
WHERE auto_id NOT IN
(SELECT min(auto_id)
FROM original_admission_predict
GROUP BY student_id);



Didnt work delete all duplicate rows instead of keeping one

DELETE FROM original_admission_predict
WHERE 
student_id IN
(SELECT student_id from
(select *, 
row_number() OVER (partition by student_id order by student_id) as rn 
FROM original_admission_predict)t
where rn > 1);

Didnt Work with inner join steps

/*DELETE
FROM original_admission_predict
WHERE 
student_id IN
(SELECT student_id from

SELECT * FROM original_admission_predict;



select * from(select *,
row_number() OVER (partition by location order by student_id) AS rn
FROM original_admission_predict) t
Having rn>1;


select * from original_

Delete from original_admission_predict t1
INNER JOIN original_admission_predict t2
ON t1.



select *, 
row_number() OVER (partition by student_id order by student_id) as rn 
FROM original_admission_predict;

DELETE t1 FROM contacts t1 INNER JOIN contacts t2  WHERE      t1.id < t2.id AND      t1.email = t2.email;*/

select * from original_admission_predict;

WITH cte AS (
    SELECT 
        *, 
        ROW_NUMBER() OVER (
            PARTITION BY 
               student_id
            ORDER BY 
                student_id
        ) row_num
     FROM 
        University.original_admission_predict
)
Select *from cte where row_num <=1;

DELETE FROM cte
WHERE row_num > 1;


/*Create view can also be used -  first create framework using select columns  and then using insert into view and add rn query

CREATE VIEW TEMP (SELECT 
        *, 
        ROW_NUMBER() OVER (
            PARTITION BY 
               student_id
            ORDER BY 
                grescore
        ) row_num
     FROM 
        University.original_admission_predict)
        Select *from cte where row_num <=1;