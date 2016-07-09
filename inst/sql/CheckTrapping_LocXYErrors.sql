# Find errors in LocX and LocY
# Errors arising from missing '.0' at end are dealt with in a seperate query
select
	id, obs, gr, date,
	LocX, LocY,
	sex, concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tagLft, tagRt,
	comments
from trapping
where
	# Exclude locs that are bad because they're missing '.0's from end
	(NOT (LocX REGEXP '^[-0-9]{1,3}$' or LocX REGEXP BINARY '^[A-V]{1}$') and
		# Find locs that aren't either:
		#	1. Up to 3 numbers (or - sign) followed by a '.0' or '.5'
		#	2. A single captial letter followed by a '.0' or '.5'
		NOT (LocX REGEXP '^[-0-9]{1,3}[[.period.]]{1}0|5$' or LocX REGEXP BINARY '^[A-V]{1}[[.period.]]{1}0|5$')
	)
	# LocY must be up to 3 numbers (or - sign) followed by a '.0' or '.5'
	or (NOT (LocY REGEXP '^[-0-9]{1,3}$') and NOT (LocY REGEXP '^[-0-9]{1,3}[[.period.]]{1}0|5$'))
order by
	obs, gr, LocX, LocY;