# DNA missing from a new squirrel or RIP/RIP
select
	id, obs, gr, date,
	ft, (case when ft = 2 then 'new tag adult' when ft = 7 then 'new tag juv' when ft = 13 then 'rip/rip' end) as fate,
	dna1, dna2,
	sex, concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tagLft, tagRt,
	comments
from trapping
where
	ft IN (2,7,13)
	and (dna1 IS NULL or dna2 IS NULL)
  and gr != 'LL';
