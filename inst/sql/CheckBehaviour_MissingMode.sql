## finds any behaviour record where the mode is missing

select id, grid, date, observer, mode, tag_left, tag_right
  from behaviour
 where mode is NULL
order by observer, date;

