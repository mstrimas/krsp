### This calculates the feed obs for the past week for each of the grids and each of the observers. 
#There are two forms for the date that can be used depending on when the query is being run

select observer, count(*) as count from behaviour 
where behaviour = 1 
and #datediff(curdate(), date) between 0 and 6
date between '2015-03-26' and '2015-04-01'
group by observer
order by observer;

select grid, count(*) as count from behaviour 
where behaviour = 1 
and #datediff(curdate(), date) between 0 and 6
date between '2015-03-26' and '2015-04-01'
group by grid
order by grid;