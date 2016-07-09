# Litter assigned to wrong grid
# note: this query does not bring up when pups are assigned to the wrong grid. If an ipod is set up to a different grid than the nest is being done on,
# you can still search for the female on the grid you are on, and enter data. However, in this case the pups would be assigned to the
# grid that the ipod is set up on (ie. the wrong grid)!

select
	l.id, l.grid, s.gr, l.LocX, l.LocY,
	concat(coalesce(s.colorlft,'-'),'/',coalesce(s.colorrt,'-')) as colours, tagLft, tagRt
from
	litter l
	join squirrel s
		on l.squirrel_id = s.id
where l.grid <> s.gr
order by s.gr, l.id;