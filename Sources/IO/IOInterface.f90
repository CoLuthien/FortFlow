module IOInterface
    use, intrinsic :: iso_fortran_env
    implicit none

contains
    ! Read Fixed input data
    subroutine read_input_table(file_name, ctrl_vars, num_vars, flow_common)
        use :: tomlf
        use :: Metrics
        character(len=*), intent(in) :: file_name
        type(ControlVars), intent(out) :: ctrl_vars
        type(NumericalVars), intent(out) :: num_vars
        type(FlowMetricsCommon), intent(out) :: flow_common
    end subroutine

end module
