# Problems with DNA vial numbers
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
	(date1 IS NOT NULL and
		(length(j.dna1) <> 8					# Vial # less than 8 characters
		or left(j.dna1,2) <> l.grid		# First 2 characters don't match grid
 		or mid(j.dna1,3,2) <> '15'		# 3rd and 4th character should be this year
 		or right(j.dna1,2) not between 1 and 81	# Last 2 characters are between 1 and 81
 		)
 	)
 	or (tagDt IS NOT NULL and
		(length(j.dna2) <> 8
		or left(j.dna2,2) <> l.grid
 		or mid(j.dna2,3,2) <> '15'
 		or right(j.dna1,2) not between 1 and 81
 		)
 	)
 order by l.grid, l.id;