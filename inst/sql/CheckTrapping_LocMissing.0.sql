# LocX and LocY look good but are missing '.0' at end
select
	id, obs, gr, date,
	LocX, LocY,
	sex, concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tagLft, tagRt,
	comments
from trapping
where
	# Locs contain no '.' and are either:
	# 	1. 3 characters long and composed of a combination of numbers and - sign (Only possibility for LocY)
	#	2. a single capital letter
	(LocX REGEXP '^[-0-9]{1,3}$' or LocX REGEXP BINARY '^[A-V]{1}$')
	or LocY REGEXP '^[-0-9]{1,3}$'
order by
	gr, LocX, LocY, date;