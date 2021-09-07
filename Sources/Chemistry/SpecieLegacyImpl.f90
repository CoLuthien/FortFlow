module SpecieLegacyImpl
    use, intrinsic :: iso_fortran_env
    use :: SpecieInterface
    implicit none

    type, extends(Specie) :: SpecieLegacy
        real(real64), dimension(7, 2) :: poly_cp, poly_h, poly_s ! mole based non-dimensinal poly-coefficients
        real(real64), dimension(7, 2) :: poly_cp_m, poly_h_m, poly_s_m ! mole based non-dimensinal poly-coefficients
        real(real64), dimension(5) :: poly_vis, poly_cd !---- Non-D of the Coeff.'s of Viscosity,Conductivity 
    end type

end module 