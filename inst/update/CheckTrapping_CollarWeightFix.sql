 # Fix collar weight issues
start transaction;

update trapping
set collWt = 4, wgt = scale_weight - bagWt - 4
where
	squirrel_id IS NOT NULL
	and radio IN (2,3,4)
	and coalesce(wgt,0) between 100 and 400
	and coalesce(collWt,0) != 8 
	and coalesce (collWt,0) != 4
	/*and id NOT IN ()*/; # Specific record id's can be put in brackets to exclude from update, separate by commas

#rollback; # Run this to undo changes, if something goes wrong with above update
commit;
