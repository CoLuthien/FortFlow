program HelloCFD
    use, intrinsic :: iso_fortran_env
    use :: Metrics
    use :: Globals
    use :: tomlf
    use :: IOInterface
    implicit none

    type(FlowMetrics) :: fuel_injection
    type(ControlInputs) :: ctrl_vars
    type(NumericalInputs) :: num_vars
    type(FlowMetricsInputs) :: flow
    type(ChemistryInputs) :: chem

    type(toml_table), allocatable :: tb
    integer :: fd

    call read_input_file("test-case.toml", ctrl_var, num_vars, flow, chem) 
    print*, ctrl_var%grid_file
end program
