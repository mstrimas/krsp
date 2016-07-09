# Long strings of numbers in detail line result from detail field not defaulting to NULL
# These numbers are just junk, overwrite them with null as necessary
start transaction;

update behaviour
set detail = NULL
where
	behaviour NOT IN (0,1,2,3,7,8,9,12,15,18)
	and detail NOT between 0 and 32; 	# No detail should ever be greater than 32,
																		# this is just a double check before overwriting

#rollback;
commit;