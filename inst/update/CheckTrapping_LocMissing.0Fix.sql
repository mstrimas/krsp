# Add '.0' to loc's that are missing them as necessary
start transaction;

# Store the records to be changed in a temporary table so they can be checked
create temporary table LocsToFix
select distinct id
from trapping
where
	(LocX REGEXP '^[-0-9]{1,3}$' or LocX REGEXP BINARY '^[A-V]{1}$')
	or LocY REGEXP '^[-0-9]{1,3}$';

# Fix LocX
update trapping
set LocX = CONCAT(TRIM(LocX),'.0')
where LocX REGEXP '^[-0-9]{1,3}$' or LocX REGEXP BINARY '^[A-V]{1}$';

# Fix LocY
update trapping
set LocY = CONCAT(TRIM(LocY),'.0')
where LocY REGEXP '^[-0-9]{1,3}$';

# Looked at changed records, if there's a problem run rollback statement below
select id, obs, gr, date, LocX, LocY
from trapping
where id IN (select id from LocsToFix)
order by gr, LocX, LocY;

#rollback; # Run this to undo changes if something goes wrong with above update
commit;
drop table LocsToFix;