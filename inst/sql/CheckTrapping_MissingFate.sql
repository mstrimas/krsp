# Find trapping records that have no fate entered
# In most cases, user forgot to enter 'recap', but also catches errors arising from web app promblem with new squirrels and RIP/RIP
# Entries from March and April 2013 arrise due to issues with drop down menues on the web app. Ignore these entries here.
select
	id, obs, gr, date, ft,
	sex, concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, color_fate, tagLft, tagRt,
	comments
from trapping
where squirrel_id IS NOT NULL and ft IS NULL
order by
	obs, gr, id;

# NOTE: We were unable to enter a fate for UTS in March due to database liitations. Ignore any records brought up by this query that date before April 1st, 2013.