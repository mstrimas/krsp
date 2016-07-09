# Missing DNA from N1 or N2
select
	l.id, l.grid, date1 as N1date, tagDt as N2date,
	l.LocX, l.LocY,
	l.mTagLft, l.mTagRt,
	j.sex, j.notch,
	j.dna1, j.dna2, j.comments
from
	juvenile j
	join litter l
		on j.litter_id = l.id
where
	(date1 IS NOT NULL and dna1 IS NULL)
	or (tagDt IS NOT NULL and dna2 IS NULL)
order by l.grid, l.id;