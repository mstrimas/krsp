# Problem colours or tags - checks a bunch of things, see below
select
	id, obs, gr, date,
	color_left, color_right, tagLft, tagRt,
	comments
from trapping
where
	squirrel_id IS NOT NULL
	and ft != 10 and ft != 12 and ft != 4 # ft 10 is natural death. ft 12 is handling death. ft 4 is trapping death. These colours should be blank.
	and (
		# Colours and tags shouldn't be blank
		color_left IS NULL or color_right IS NULL or tagLft IS NULL or tagRt IS NULL
		# Colours only have allowable characters
		# BINARY makes this case sensitive; could add other allowable characters between [], but don't remove ^
		or color_left REGEXP BINARY '[^-!*BGOPRWYk]' or color_right REGEXP BINARY '[^-!*BGOPRWYk]'
		# Tags are 5 characters long and composed of #'s and DFHJM
		# BINARY makes this case sensitive
		# could add other allowable characters between [], e.g. if we later get K-series tags
		or (tagLft <> '-' and tagLft NOT REGEXP BINARY '^[MDFHJ0-9]{5}$')
		or (tagRt <> '-' and tagRt NOT REGEXP BINARY '^[MDFHJ0-9]{5}$')
		)
order by obs, gr, id;
