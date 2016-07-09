# DNA vial number problems
# Checks that:
# 	1. First 2 characters match grid name
#		2. Vial number is 8 characters long
#		3. Year is correct
select
	id, obs, gr, date,
	sex, concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tagLft, tagRt,
	dna1, dna2,
	comments
from trapping
where
	(dna1 IS NOT NULL and (left(dna1,2) <> gr or length(dna1) <> 8 or mid(dna1,3,2) <> '15')) # '15' refers to year, can change
	or (dna2 IS NOT NULL and (left(dna2,2) <> gr or length(dna2) <> 8 or mid(dna1,3,2) <> '15')) # '15' refers to year, can change
order by
	obs, gr, id;
