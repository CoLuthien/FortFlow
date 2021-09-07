module SpecieInterface
    use, intrinsic :: iso_fortran_env
    implicit none

    type, abstract :: Specie
        ! input properties
        real(real64) :: molar_weight  ![kg/mol]
        real(real64) :: molar_hof ! [KJ/mol] @ 0K, table could be implementation dependent
        real(real64) :: molar_diam ! [Angstrom] molecular diameter
        real(real64) :: char_temp ! charateristic temperature for molecular 

        ! calculated properties
        real(real64) :: molar_weight_nd
        real(real64) :: mass_hof ! heat of formation
        real(real64), allocatable, private :: f1(:), f2(:) ! by doi:10.2514/6.2013-303, pre-calculated weight for viscosity coefficient calculation
        real(real64), allocatable, private :: teab(:) ! unknown
        real(real64), allocatable, private :: diff_coef(:) ! precalculated part of diffusion coefficient eqn
    end type Specie

contains



end module
