# Look for bad Loc's
#FES - this query is not very usefull. It brings up locs missing ".0", but the database drops these ".0"s even when we enter them.
select
	id, observer, grid, date, time,
	concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tag_left, tag_right,
	LocX, LocY,
	comments
from behaviour
where
	# Find locs that aren't either:
	#	1. Up to 3 numbers (or - sign) followed by a '.' and a single number
	#	2. A single captial letter followed by a '.' and a single number
	NOT (LocX REGEXP '^[-0-9]{1,3}[[.period.]]{1}[0-9]{1}$' or LocX REGEXP BINARY '^[A-V]{1}[[.period.]]{1}[0-9]{1}$')
	# LocY must be: up to 3 numbers (or - sign) followed by a '.' and a single number
	# Also, accept a missing decimal since web app drops trailing '.0', could comment second bit out
	or NOT (LocY REGEXP '^[-0-9]{1,3}[[.period.]]{1}[0-9]{1}$' or LocY REGEXP '^[-0-9]{1,3}$')
order by
	observer, grid, LocX, LocX;