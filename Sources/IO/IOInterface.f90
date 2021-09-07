module IOInterface
    use, intrinsic :: iso_fortran_env
    use :: Globals
    implicit none

contains
    ! Read Fixed input data
    subroutine read_input_file(file_name, ctrl, num, flow, chem)
        use :: tomlf
        character(len=*), intent(in) :: file_name
        type(ControlInputs), intent(out) :: ctrl
        type(NumericalInputs), intent(out) :: num
        type(FlowMetricsInputs), intent(out) :: flow
        type(ChemistryInputs), intent(out) :: chem
        type(toml_table), allocatable :: root
        integer :: fd
        fd = open_file(file_name)

        call toml_parse(root, fd)

        call read_control_inputs(root, ctrl)

        call read_numerical_inputs(root, num)

        call read_flow_inputs(root, flow, "Inflow")

        call read_chemistry_inputs(root, chem)

        close(fd)

        deallocate (root)
    end subroutine

    subroutine read_chemistry_inputs(root, data)
        use :: tomlf
        type(toml_table), intent(inout) :: root
        type(ChemistryInputs), intent(out) :: data
        type(toml_table), pointer :: target
        character(len=:), allocatable :: ptr

        call get_value(root, "ChemistryInputs", target)

        call get_value(target, "thermo-table", data%thermo_table)
        call get_value(target, "trans-table", data%trans_table)
        call get_value(target, "reaction-model", data%reaction_table)
    end subroutine

    subroutine read_flow_inputs(root, data, key)
        use :: tomlf
        type(toml_table), intent(inout) :: root
        type(FlowMetricsInputs), intent(out) :: data
        character(len=*), intent(in) :: key
        type(toml_table), pointer :: common, target
        type(toml_array), pointer :: spcs, mixture
        character(len=:), allocatable :: ptr
        integer :: length, idx

        call get_value(root, "FlowFieldInputs", common); 
        call get_value(common, key, target); 
        call get_value(common, "length-scale", data%length_scale)
        call get_value(common, "cdl", data%cdl)
        call get_value(common, "ctk", data%ctk)
        call get_value(common, "ctw", data%ctw)

        call get_value(target, "mach", data%mach)
        call get_value(target, "pressure", data%mach)
        call get_value(target, "temp", data%mach)

        call get_value(target, "mixture-name", spcs)

        call get_value(target, "mixture-value", mixture)

        length = len(mixture)
        allocate (data%name(length), data%value(length))

        do idx = 1, length
            call get_value(spcs, idx, ptr)
            call get_value(mixture, idx, data%value(idx))
            data%name(idx) = ptr
        end do
    end subroutine

    subroutine read_numerical_inputs(root, data)
        use :: tomlf
        type(toml_table), intent(inout) :: root
        type(NumericalInputs), intent(out) :: data
        type(toml_table), pointer :: numeric, scheme
        character(len=:), allocatable :: name

        call get_value(root, "NumericalInputs", numeric)
        call get_value(numeric, "Scheme", scheme)

        call get_value(numeric, "absolute-epsilone", data%abs_eps)
        call get_value(numeric, "relative-epsilone", data%rel_eps)
        call get_value(numeric, "machine-epsilone", data%machine_epsilon)
        call get_value(numeric, "cfl", data%CFL)

        call get_value(scheme, "epsilone", data%eps)
        call get_value(scheme, "theta", data%theta)

    end subroutine

    subroutine read_control_inputs(root, data)
        use :: tomlf
        type(toml_table), intent(inout) :: root
        type(ControlInputs), intent(out) :: data
        type(toml_table), pointer :: ctrl_table
        character(len=:), allocatable :: name

        call get_value(root, "ControlInputs", ctrl_table)

        call get_value(ctrl_table, "case-name", name)
        call move_alloc(name, data%case_name)

        call get_value(ctrl_table, "grid-name", name)
        call move_alloc(name, data%grid_file)

        call get_value(ctrl_table, "iter-end", data%iter_end)
        call get_value(ctrl_table, "write-interval", data%write_interval)

        call get_value(ctrl_table, "nx", data%nx)
        call get_value(ctrl_table, "ny", data%ny)
        call get_value(ctrl_table, "nz", data%nz)

        call get_value(ctrl_table, "restart", data%restart)
    end subroutine

    function open_file(file_name) result(fd)
        character(len=*), intent(in) :: file_name
        integer :: fd, stat
        fd = 0
        open (newunit=fd, file="test-case.toml", status='OLD', iostat=stat)
        if (stat /= 0) then
            print *, "open file name", file_name, "failed"
        end if
    end function
end module
