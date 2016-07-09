# Find behaviours at odd times
# Specifically behaviours not between 6am and 8pm
# Run code to fix times b/t midnight and 1am before this
select
	id, observer, grid, date, time, mode,
	concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tag_left, tag_right,
	LocX, LocY,
	behaviour, detail,
	comments
from
	behaviour
where
	time not between '06:00:00' and '22:00:00'
and mode != 4
order by
	observer, grid, time;