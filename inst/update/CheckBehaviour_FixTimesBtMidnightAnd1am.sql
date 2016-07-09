# Web app doesn't handle times between noon and 1pm correctly, it shifts them to be between midnight and 1am
# Find these problem times and fix them
start transaction;

# Find behaviour times between midnight and 1am
select
	id, observer, grid, date, time, mode,
	concat(coalesce(color_left,'-'),'/',coalesce(color_right,'-')) as colours, tag_left, tag_right,
	LocX, LocY,
	behaviour, detail,
	comments
from
	behaviour
where
	time between '00:00:00' and '01:00:00'
and mode != '4'
order by time;

# Shift times between midnight and 1am by 12 hours
update behaviour
set time = ADDTIME(time, '12:00:00')
where time between '00:00:00' and '01:00:00';
and mode != 4
#rollback;
commit;