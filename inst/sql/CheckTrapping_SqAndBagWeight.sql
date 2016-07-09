# Problem with squirrel and bag weights
select
	id, obs, gr, date,
	wgt, scale_weight, bagWt, collWt,
	sex, concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tagLft, tagRt,
	comments
from trapping
where
	squirrel_id IS NOT NULL
	and coalesce(wgt,0) <> 0
	and (
		# Change tolerance for weights here
		NOT (wgt between 70 and 390)
		or NOT (coalesce(bagWt,0) between 75 and 170)
		)
  and date != '2014-05-10'
order by obs, gr, date;
