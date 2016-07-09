# Missing radio collar weights
select
	id, obs, gr, date,
	radio, (case when radio = 2 then 'rc on' when radio = 3 then 'rc off' when radio = 4 then 'rc change' end) as CollarFate,
	collWt, wgt, scale_weight, bagWt,
	sex, concat(color_left,'/',color_right) as colours, tagLft, tagRt,
	comments
from trapping
where
	squirrel_id IS NOT NULL
	and radio IN (2,3,4)
	and coalesce(wgt,0) between 100 and 400
	and coalesce(collWt,0) != 8 
	and coalesce (collWt,0) != 4
order by
	obs, gr, date;