program HelloCFD
    use, intrinsic :: iso_fortran_env
    use :: Globals
    use :: tomlf
    use :: IOInterface
    use :: ArrayBase
    use :: CudaArray
    use :: CudaCellMetrics
    use :: Vector
    implicit none

    type(FlowMetrics) :: fuel_injection
    type(ControlInputs) :: ctrl_vars
    type(NumericalInputs) :: num_vars
    type(FlowMetricsInputs) :: flow
    type(ChemistryInputs) :: chem
    type(Array3) :: x, y ,z 
    type(CudaCellMetric3) :: metric

    type(toml_table), allocatable :: tb
    integer :: fd
    integer :: res(3)

    res = 100
    
    x = Array3([0, 0, 0], res + 2)
    y = Array3([0, 0, 0], res + 2)
    z = Array3([0, 0, 0], res + 2)

    call read_input_file("test-case.toml", ctrl_var, num_vars, flow, chem) 
    print*, ctrl_var%grid_file

    metric = CudaCellMetric3(x, y, z, res) 

    pause
end program
