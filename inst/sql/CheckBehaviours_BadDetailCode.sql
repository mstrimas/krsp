# Only a subset of behaviours should have detail codes associated with them
# Check to ensure that details haven't been assigned to behaviours that shouldn't have them
# Most of these will be errors associated with the detail field not defaulting to NULL
select
	id, observer, grid, date, time,
	concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tag_left, tag_right,
	LocX, LocY,
	behaviour, detail,
	comments
from
	behaviour
where
	behaviour NOT IN (0,1,2,3,7,8,9,12,15,18)
	and NOT (detail IS NULL or detail = 0)
order by
	grid, behaviour;