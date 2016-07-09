### There are 5 checks in this query####


# Look for bad LocX or LocY. 
# These Should be the reflo of the midden and are often confused with the nest loc
select id, grid, ln as LitNum, LocX, LocY
from litter
where
	# Find locs that aren't either:
	#	1. Up to 3 numbers (or - sign) followed by a '.0' or '.5'
	#	2. A single captial letter followed by a '.0' or '.5'
	NOT (LocX REGEXP '^[-0-9]{1,3}[[.period.]]{1}0|5$' or LocX REGEXP BINARY '^[A-V]{1}[[.period.]]{1}0|5$')
	# LocY must be: up to 3 numbers (or - sign) followed by a '.0' or '.5'
	# Also, accept a missing decimal since web app drops trailing '.0', could comment second bit out
	or NOT (LocY REGEXP '^[-0-9]{1,3}[[.period.]]{1}0|5$' or LocY REGEXP '^[-0-9]{1,3}$')
order by
	grid, LocX, LocY;



# Look for bad N1 Loc's.
select id, grid, ln as LitNum, nx1 as N1LocX, ny1 as N1LocY
from litter
where
	# Find locs that aren't either:
	#	1. Up to 3 numbers (or - sign) followed by a '.' and a single number
	#	2. A single captial letter followed by a '.' and a single number
	NOT (nx1 REGEXP '^[-0-9]{1,3}[[.period.]]{1}[0-9]{1}$' or nx1 REGEXP BINARY '^[A-V]{1}[[.period.]]{1}[0-9]{1}$')
	# LocY must be: up to 3 numbers (or - sign) followed by a '.' and a single number
	# Also, accept a missing decimal since web app drops trailing '.0', could comment second bit out
	or NOT (ny1 REGEXP '^[-0-9]{1,3}[[.period.]]{1}[0-9]{1}$' or ny1 REGEXP '^[-0-9]{1,3}$')
order by
	grid, nx1, ny1;


# Look for bad N2 Loc's
select id, grid, ln as LitNum, nx2 as N2LocX, ny2 as N2LocY
from litter
where
	# Find locs that aren't either:
	#	1. Up to 3 numbers (or - sign) followed by a '.' and a single number
	#	2. A single captial letter followed by a '.' and a single number
	NOT (nx2 REGEXP '^[-0-9]{1,3}[[.period.]]{1}[0-9]{1}$' or nx2 REGEXP BINARY '^[A-V]{1}[[.period.]]{1}[0-9]{1}$')
	# LocY must be: up to 3 numbers (or - sign) followed by a '.' and a single number
	# Also, accept a missing decimal since web app drops trailing '.0', could comment second bit out
	or NOT (ny2 REGEXP '^[-0-9]{1,3}[[.period.]]{1}[0-9]{1}$' or ny2 REGEXP '^[-0-9]{1,3}$')
order by
	grid, nx2, ny2;



# Look for missing nest 1 Locs
select id, grid, locx, locy, mTagLft, mTagRt, nx1,ny1
from litter 
where
	# This looks for records that breeding status has been recorded as YES but the loc of the nest has not been filled in.
br = 1 and date1 is not NULL and nx1 is NULL
or br = 1 and date1 is not NULL and ny1 is NULL;

# Finds missing locs for N2, the query uses breeding status of YES and no nest 2 locs input 27 days after field birth date
# This will bring up those missing data and any nests that have been forgotten.
select id, grid, locx, locy, mTagLft, mTagRt, nx2,ny2, comments
from litter 
where
	# This looks for records that breeding status has been recorded as YES but the loc of the nest has not been filled in.
DATEDIFF(CURDATE(),fieldBDate) > 27 
and br = 1 and nx2 is NULL
or 
DATEDIFF(CURDATE(),fieldBDate) > 27 
and br = 1 and ny2 is NULL;


