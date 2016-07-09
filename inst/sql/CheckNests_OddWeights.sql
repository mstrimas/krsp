# Check for odd weights; most hits will be from late nests
# N1's less than 7g or greater than 25g
# N2's less than 30g or greater than 80g
# Tolerance can be easily changed below
select
	l.id, l.grid, date1 as N1date, tagDt as N2date,
	l.LocX, l.LocY,
	l.mTagLft, l.mTagRt,
	j.sex, j.notch,
	j.weight, j.tagWt,
	j.comments
from
	juvenile j
	join litter l
		on j.litter_id = l.id
where
	j.weight not between 7 and 25
	or j.tagWt not between 30 and 80
order by l.grid, l.id;