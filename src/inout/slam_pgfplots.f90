!=============================================================================================
!
!!> @file     slam_pgfplots.f90
!!
!!  @anchor   module_pgfplots
!!
!>  @brief    Providing routines for the generation of PGF/TikZ plots from Fortran programs
!!
!>  @author   Christopher Kebschull
!>  @author   Vitali Braun
!!
!>  @date     <ul>
!!              <li> 10.07.2015 (assimilated into libslam)</li>
!!              <li> 27.04.2015 (initial design)</li>
!!            </ul>
!!
!>  @details  This module contains parameters, subroutines and functions which allow for an
!!            automated generation of .tex files which can later be processed with LaTeX to
!!            generate plots using PGF/TikZ.
!!
!>  @copyright Institute of Space Systems / TU Braunschweig
!!
!----------------------------------------------------------------------------------------------
module slam_pgfplots

  use slam_io
  use slam_error_handling
  use slam_types

  implicit none

  private

  character(len=*), parameter :: CREATE_PREAMBLE  = 'PREAMBLE'
  character(len=*), parameter :: documentPreamble = '\documentclass{article}'//new_line('a')//new_line('a')// &
                                                    '  \usepackage[english]{babel}'//new_line('a')// &
                                                    '  \usepackage[usenames,dvipsnames,svgnames,table]{xcolor}'//new_line('a')// &
                                                    '  \usepackage{tikz}'//new_line('a')// &
                                                    '  \usepackage{pgfplots}'//new_line('a')// &
                                                    '  \usepackage{pgfplotstable}'//new_line('a')// &
                                                    '    \usetikzlibrary{calc,%'//new_line('a')// &
                                                    '                    matrix,%'//new_line('a')// &
                                                    '                    shapes,%'//new_line('a')// &
                                                    '                    decorations,%'//new_line('a')// &
                                                    '                    backgrounds,%'//new_line('a')// &
                                                    '                    fit,%'//new_line('a')// &
                                                    '                    arrows,%'//new_line('a')// &
                                                    '                    positioning,%'//new_line('a')// &
                                                    '                    trees,%'//new_line('a')// &
                                                    '                    shadows,%'//new_line('a')// &
                                                    '                    intersections,%'//new_line('a')// &
                                                    '                    pgfplots.groupplots}'//new_line('a')//new_line('a')

  integer, parameter :: LEN_CONFIG_STRING = 255 !< maximum length of configuration string
  integer, parameter :: MAX_PLOTS = 100 !< maximum number of plots

  integer, dimension(MAX_PLOTS) :: ids = -1  !< array for managing plot ids: ID corresponds to array element
                                             !< i.e. ids[1] is associated with the ID '1'. If the array contains a '-1',
                                             !< the ID is available.
  integer, dimension(MAX_PLOTS) :: plotChannel = -1  !< containing connected plot channels

  character(len=LEN_CONFIG_STRING), dimension(MAX_PLOTS) :: plotConfig = "" !< containing CSV configuration string for each plot

  public :: addPlot
  public :: createPlot
  public :: finishPlot

contains

  !==================================================================================================
  !
  !>  @brief    Create a new plot - which may contain a configurable number of subplots. Returns a plot id to be used for later module calls.
  !>  @author   Vitali Braun
  !!
  !>  @date     <ul>
  !!              <li> 27.04.2015 (initial design)</li>
  !!            </ul>
  !!
  !>  @param[in]  fileName        Name of .tex file to generate
  !>  @param[in]  subPlots        Number of subplots added to the plot (optional)
  !>  @param[in]  scaledTicks     Tick scaling (0.0000012 -> 1.2 and 10-6 above the plot)  (optional)
  !>  @param[in]  width           Plot width  (e.g. '0.5\textwidth')
  !>  @param[in]  height          Plot height (e.g. '0.5\textwidth')
  !>  @param[in]  xmin            Minimum value on x axis (optional)
  !>  @param[in]  xmax            Maximum value on x axis (optional)
  !>  @param[in]  ymin            Minimum value on y axis (optional)
  !>  @param[in]  ymax            Maximum value on y axis (optional)
  !>  @param[in]  xlabel          Label for x axis (optional)
  !>  @param[in]  ylabel          Label for y axis (optional)
  !>  @param[in]  xtick           Appearance of ticks on x-axis, e.g. '1,...,5' or '1,2,3,4'
  !>  @param[in]  mxtick          Appearance of minor ticks on x-axis, e.g. '1,...,5' or '1,2,3,4'
  !>  @param[in]  ytick           Appearance of ticks on y-axis, e.g. '1,...,5' or '1,2,3,4'
  !>  @param[in]  mytick          Appearance of minor ticks on y-axis, e.g. '1,...,5' or '1,2,3,4'
  !>  @param[in]  legendEntries   Legend entries for subplots (optional)
  !>  @param[in]  legendStyle     Legend style (optional)
  !>  @param[in]  grid            Grid appearance ('minor'||'major'||'both') (optional)
  !>  @param[in]  writePreamble   Generates a document preamble (optional)
  !!
  !----------------------------------------------------------------------------------------------
  integer function createPlot(fileName, subPlots, scaledTicks, &
                              xmin, xmax, ymin, ymax, &
                              xlabel, ylabel, &
                              width, height, &
                              xtick, ytick, mxtick, mytick, &
                              legendEntries, legendStyle, grid, &
                              writePreamble) result(id)

    character(len=*), intent(in)  :: fileName
    character(len=*), intent(in), optional :: xlabel
    character(len=*), intent(in), optional :: ylabel
    logical,  intent(in), optional :: writePreamble
    integer,  intent(in), optional :: subPlots
    logical,  intent(in), optional :: scaledTicks
    real(dp), intent(in), optional :: xmin
    real(dp), intent(in), optional :: xmax
    real(dp), intent(in), optional :: ymin
    real(dp), intent(in), optional :: ymax
    character(len=*), intent(in), optional :: legendEntries
    character(len=*), intent(in), optional :: legendStyle
    character(len=*), intent(in), optional :: width
    character(len=*), intent(in), optional :: height
    character(len=*), intent(in), optional :: xtick
    character(len=*), intent(in), optional :: mxtick
    character(len=*), intent(in), optional :: ytick
    character(len=*), intent(in), optional :: mytick
    character(len=*), intent(in), optional :: grid

    character(len=*), parameter      :: csubid = 'createPlot'
    character(len=*), parameter      :: indent = repeat(' ', 14)
    character(len=LEN_CONFIG_STRING) :: currentConfig

    integer :: i

    logical :: foundId

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    !** provide an ID for the plot and open file channel
    foundId = .false.

    do i = 1, MAX_PLOTS

      if(ids(i) < 0) then
        id     = i
        ids(i) = i
        plotChannel(id) = openFile(fileName, SEQUENTIAL, OUT_FORMATTED_OVERWRITE)
        if(hasFailed()) return
        foundId = .true.
        exit
      end if

    end do

    if(.not. foundId) then
      call setError(E_PLOT_CREATE, FATAL)
      return
    end if

    !** write document preamble on request
    if(present(writePreamble)) then
      if(writePreamble) then

        !** append to configuration string
        currentConfig = plotConfig(id)
        write(plotConfig(id),'(a)') trim(adjustl(currentConfig))//CREATE_PREAMBLE//","

        !** write preamble
        write(plotChannel(id),'(a)') documentPreamble

        !** write begin statements
        write(plotChannel(id),'(a)') '\begin{document}'
        write(plotChannel(id),'(a)') '  \begin{tikzpicture}'
        write(plotChannel(id),'(a)') '    \begin{axis}['

      end if
    else  ! write only begin statements

      write(plotChannel(id),'(a)') '\begin{tikzpicture}'
      write(plotChannel(id),'(a)') '  \begin{axis}['

    end if

    ! add options

    if(present(width)) then
      write(plotChannel(id),'(a,e9.3e2,a)') indent//'width = ', width ,','
    end if

    if(present(height)) then
      write(plotChannel(id),'(a,e9.3e2,a)') indent//'height = ', height ,','
    end if

    if(present(scaledTicks)) then
      if(scaledTicks) then
        write(plotChannel(id),'(a)') indent//'scaled ticks = true,'
      else
        write(plotChannel(id),'(a)') indent//'scaled ticks = false,'
      end if
    end if

    if(present(xmin)) then
      write(plotChannel(id),'(a,e9.3e2,a)') indent//'xmin = ', xmin ,','
    end if

    if(present(xmax)) then
      write(plotChannel(id),'(a,e9.3e2,a)') indent//'xmax = ', xmax ,','
    end if

    if(present(xtick)) then
      write(plotChannel(id),'(a)') indent//'xtick = {'//xtick//'},'
    end if

    if(present(mxtick)) then
      write(plotChannel(id),'(a)') indent//'minor xtick = {'//mxtick//'},'
    end if

    if(present(ymin)) then
      write(plotChannel(id),'(a,e9.3e2,a)') indent//'ymin = ', ymin ,','
    end if

    if(present(ymax)) then
      write(plotChannel(id),'(a,e9.3e2,a)') indent//'ymax = ', ymax ,','
    end if

    if(present(ytick)) then
      write(plotChannel(id),'(a)') indent//'ytick = {'//ytick//'},'
    end if

    if(present(mytick)) then
      write(plotChannel(id),'(a)') indent//'minor ytick = {'//mytick//'},'
    end if

    if(present(xlabel)) then
      write(plotChannel(id),'(a)') indent//'xlabel = '//xlabel//','
    end if

    if(present(ylabel)) then
      write(plotChannel(id),'(a)') indent//'ylabel = '//ylabel//','
    end if

    if(present(legendEntries)) then
      write(plotChannel(id),'(a)') indent//'legend entries = {'//legendEntries//'},'
    end if

    if(present(legendStyle)) then
      write(plotChannel(id),'(a)') indent//'legend style = {'//legendStyle//'},'
    end if

    if(present(grid)) then
      write(plotChannel(id),'(a)') indent//'grid = '//grid//','
    end if

    ! close option paranthesis
    write(plotChannel(id),'(a)')  indent//']'

    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end function createPlot

  !==================================================================================================
  !
  !>  @brief    Add a plot to an existing tikzpicture
  !!
  !>  @author   Vitali Braun
  !!
  !>  @date     <ul>
  !!              <li> 28.04.2015 (initial design)</li>
  !!            </ul>
  !!
  !>  @param[in]  id            Plot ID
  !>  @param[in]  plotType      Plot type, being one of the following: 'TABLE', 'COORDINATES', 'FUNCTION'
  !>  @param[in]  tableName     Name of table file (if 'TABLE' type selected) (optional)
  !>  @param[in]  coords        Coordinates array (if 'COORDINATES' type selected) (optional)
  !>  @param[in]  plotFunction  Function string (if 'FUNCTION' type selected (optional)
  !>  @param[in]  style         Optional style of plot (optional)
  !!
  !----------------------------------------------------------------------------------------------
  subroutine addPlot(id, plotType, tableName, coords, plotFunction, style)

    integer,          intent(in) :: id
    character(len=*), intent(in) :: plotType
    character(len=*), intent(in), optional :: plotFunction
    character(len=*), intent(in), optional :: style
    character(len=*), intent(in), optional :: tableName
    real(dp), dimension(:,:), intent(in), optional :: coords

    character(len=*), parameter      :: csubid = 'addPlot'
    character(len=*), parameter      :: indent = repeat(' ', 6)

    integer :: idLocal
    integer :: j

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    !** check array bounds
    if(id < 1 .or. id > MAX_PLOTS) then
      call setError(E_OUT_OF_BOUNDS, FATAL)
      return
    end if

    !** check first, if there already is an existing plot for the given id - if not, create one
    if(ids(id) <= 0) then
      idLocal = createPlot('temp.tex')
    else
      idLocal = id
    end if

    !** now write plot statement
    if(.not. present(style)) then
      write(plotChannel(idLocal),'(a)', advance='no') indent//'\addplot '
    else
      write(plotChannel(idLocal),'(a)', advance='no') indent//'\addplot['//style//'] '
    end if

    select case(plotType)
      case('TABLE')

        if(.not. present(tableName)) then
          call setError(E_MISSING_PARAMETER, FATAL, (/'tableName'/))
          return
        else
          write(plotChannel(idLocal),'(a)') 'table{'//tableName//'};'
        end if

      case('COORDINATES')

        if(.not. present(coords)) then
          call setError(E_MISSING_PARAMETER, FATAL, (/'coords'/))
          return
        else
          write(plotChannel(idLocal),'(a)') 'coordinates{'
          do j = 1, size(coords,1)
            write(plotChannel(idLocal),'(a,x,e12.5e2,a,x,e12.5e2,a)') indent//'(', coords(j,1),',', coords(j,2),')'
          end do
          write(plotChannel(idLocal),'(a)') indent//'};'
        end if

      case('FUNCTION')

        if(.not. present(plotFunction)) then
          call setError(E_MISSING_PARAMETER, FATAL, (/'plotFunction'/))
          return
        else
          write(plotChannel(idLocal),'(a)') '{'//plotFunction//'};'
        end if

      case default
        call setError(E_ID_MATCH, FATAL)
        return

    end select

    if(isControlled()) then
      call checkOut(csubid)
    end if

  end subroutine addPlot

  !==================================================================================================
  !
  !>  @brief      Finishing plots by adding \end statements and freeing ID
  !>  @author     Vitali Braun
  !!
  !>  @date       <ul>
  !!                <li> 27.04.2015 (initial design)</li>
  !!              </ul>
  !!
  !>  @param[in]  id  Id of plot to finish
  !!
  !----------------------------------------------------------------------------------------------
  subroutine finishPlot(id)

    integer, intent(in) :: id

    character(len=*), parameter :: csubid = 'finishPlot'

    if(isControlled()) then
      if(hasToReturn()) return
      call checkIn(csubid)
    end if

    !** check array bounds
    if(id < 1 .or. id > MAX_PLOTS) then
      call setError(E_OUT_OF_BOUNDS, FATAL)
      return
    end if

    !** finish current plot
    if(ids(id) > 0) then  ! do nothing, if ID not available

      if(index(plotConfig(id),CREATE_PREAMBLE) /= 0) then
        write(plotChannel(id),'(a)') '    \end{axis}'
        write(plotChannel(id),'(a)') '  \end{tikzpicture}'
        write(plotChannel(id),'(a)') '\end{document}'
      else
        write(plotChannel(id),'(a)') '  \end{axis}'
        write(plotChannel(id),'(a)') '\end{tikzpicture}'
      end if

    end if

    !** finally free ID array element
    ids(id) = -1

    if(isControlled()) then
      call checkOut(csubid)
    end if

    return

  end subroutine finishPlot

end module slam_pgfplots
