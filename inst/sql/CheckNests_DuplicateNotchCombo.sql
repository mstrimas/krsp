# Look for duplicate notch combos
select
	l.id, l.grid, date1 as N1date,
	l.LocX, l.LocY,
	l.mTagLft, l.mTagRt,
	j.sex, j.notch, j.num
from
	(select litter_id, sex, notch, count(*) as num
	from juvenile
	group by id, sex, notch
	having count(*) > 1) j
	join litter l
		on j.litter_id = l.id;
