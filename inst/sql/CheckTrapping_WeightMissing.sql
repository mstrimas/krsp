# Finds any trapping record where wgt is missing
select
	id, obs, gr, date, wgt, scale_weight, bagWt,
	sex, concat(color_left,'/',color_right) as colours, tagLft, tagRt,
	comments
from trapping
where
	wgt = 0 
	and rep_con IS NOT NULL
	and comments != 'rel' 
order by
	obs, gr, date;