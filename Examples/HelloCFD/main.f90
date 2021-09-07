program HelloCFD
    use, intrinsic :: iso_fortran_env
    use :: Metrics
    use :: tomlf
    use :: IOInterface
    implicit none

    type(FlowMetrics) :: fuel_injection
    type(ControlVars) :: ctrl_vars
    type(NumericalVars) :: num_vars
    type(FlowMetricsCommon) :: flow_cmn

    type(toml_table), allocatable :: tb
    integer :: fd

    open (newunit=fd, file="test-case.toml")

    call toml_parse(tb, fd)

    call read_input_table("test-case.toml", )

end program
