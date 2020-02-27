program countdown
  use mod_datetime, only: datetime, current_time, timedelta, get_date_from_cli
  implicit none
  type(datetime) :: now, birthday
  type(timedelta) :: td
  call get_date_from_cli(birthday)
  now = current_time()
  td = birthday - now
  if (all([now % month, now % day] &
    == [birthday % month, birthday % day])) then
    print *, 'Happy Birthday!'
  else
    print '(i3, a, 3(i2, a))', td % days, ' days, ',&
                               td % hours, ' hours, ',&
                               td % minutes, ' minutes, and ',&
                               td % seconds, ' seconds remaining until your Birthday!'
  end if
end program countdown
