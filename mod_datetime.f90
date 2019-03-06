module mod_datetime

  use iso_fortran_env, only: real64
      
  implicit none

  private
  public :: current_time, datetime, get_date_from_cli, timedelta

  type :: datetime
    integer :: year, month, day
    integer :: hour = 0, minute = 0, second = 0
  contains
    procedure, pass(d1) :: datetime_minus_datetime
    generic :: operator(-) => datetime_minus_datetime
  end type datetime

  type :: timedelta
    integer :: days = 0, hours = 0, minutes = 0, seconds = 0
  end type timedelta

contains

  pure elemental type(timedelta) function datetime_minus_datetime(d1, d2) result(t)

    class(datetime),intent(in) :: d1, d2
    real(real64) :: days_diff, ndays1, ndays2, sgn
    integer :: days, hours, minutes, seconds, year

    real, parameter :: d2h = 24.       ! day -> hour
    real, parameter :: h2d = 1. / d2h  ! hour -> day
    real, parameter :: d2m = d2h * 60  ! day -> minute
    real, parameter :: m2d = 1. / d2m  ! minute -> day
    real, parameter :: s2d = m2d / 60. ! second -> day
    real, parameter :: d2s = 1. / s2d  ! day -> second

    ndays1 = 1.0_real64 &
           * sum([(days_in_year(year), year = 1, d1 % year - 1)])&
           + yearday(d1 % year, d1 % month, d1 % day)            &
           + d1 % hour * h2d                                     &
           + d1 % minute * m2d                                   &
           + d1 % second * s2d

    ndays2 = 1.0_real64 &
           * sum([(days_in_year(year), year = 1, d2 % year - 1)])&
           + yearday(d2 % year, d2 % month, d2 % day)            &
           + d2 % hour * h2d                                     &
           + d2 % minute * m2d                                   &
           + d2 % second * s2d

    days_diff = ndays1 - ndays2

    sgn = sign(1.0_real64, days_diff)

    days = int(abs(days_diff))
    hours = int((abs(days_diff) - days) * d2h)
    minutes = int((abs(days_diff) - days - hours * h2d) * d2m)
    seconds = int((abs(days_diff) - days - hours * h2d & 
                 - minutes * m2d) * d2s)

    t = timedelta(sgn * days, sgn * hours, sgn * minutes, sgn * seconds)

  end function datetime_minus_datetime

  pure elemental integer function days_in_month(month, year)
    ! Given input month and year, 
    ! returns the number of days in the month.
    integer, intent(in) :: month, year
    integer, parameter :: days(*) = [31, 28, 31, 30, 31, 30, &
                                     31, 31, 30, 31, 30, 31]
    if (month < 1 .or. month > 12) then
      days_in_month = 0
    else if (month == 2 .and. is_leap_year(year)) then
      days_in_month = 29
    else
      days_in_month = days(month)
    end if
  end function days_in_month

  pure elemental integer function days_in_year(year)
    ! Given input year, returns the number of days in year.
    integer, intent(in) :: year
    if (is_leap_year(year)) then
      days_in_year = 366
    else
      days_in_year = 365
    end if
  end function days_in_year

  subroutine get_date_from_cli(date)
    ! Reads year, month, and day from the command line
    ! and returns the corresponding datetime instance.
    type(datetime), intent(out) :: date
    character(len=4) :: year_arg
    character(len=2) :: month_arg, day_arg
    integer :: year, month, day
    
    if (command_argument_count() < 3) then
      stop 'Usage: countdown YEAR MONTH DAY'
    end if

    call get_command_argument(1, year_arg)
    call get_command_argument(2, month_arg)
    call get_command_argument(3, day_arg)

    read(year_arg, *) year
    read(month_arg, *) month
    read(day_arg, *) day

    if (year < 1) then
      stop 'YEAR must be >= 1'
    else if (month < 1 .or. month > 12) then
      stop 'MONTH must be >= 1 and <= 12'
    else if (day < 1 .or. day > days_in_month(month, year)) then
      stop 'invalid value for DAY'
    end if

    date = datetime(year, month, day, 0, 0, 0)

  end subroutine get_date_from_cli

  pure elemental logical function is_leap_year(year)
    ! Returns .true. if input year is a leap year,
    ! and .false. otherwise.
    integer, intent(in) :: year
    is_leap_year = (mod(year, 4) == 0 .and. .not. mod(year, 100) == 0)&
              .or. (mod(year, 400) == 0)
  end function is_leap_year

  type(datetime) function current_time() result(res)
    integer :: values(8)
    call date_and_time(values=values)
    res = datetime(year = values(1), month = values(2),&
                   day = values(3), hour = values(5),&
                   minute = values(6), second = values(7))
  end function current_time

  pure elemental integer function yearday(year, month, day)
    ! Given input year, month, and day,
    ! returns the number of day in the year.
    integer, intent(in) :: year, month, day 
    integer :: m
    yearday = sum([(days_in_month(m, year), m = 1, month - 1)]) + day
  end function yearday

end module mod_datetime
