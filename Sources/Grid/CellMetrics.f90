module CellMetrics
    use, intrinsic :: iso_fortran_env
    use :: ArrayBase
    implicit none


    type :: CellMetric3
        integer :: m_resolution(3)
        type(Array3), allocatable :: aj
        type(Array3), allocatable :: sx, sy, sz
        type(Array3), allocatable :: ex, ey, ez
        type(Array3), allocatable :: cx, cy, cz
        type(Array3), allocatable :: sxc, syc, szc
        type(Array3), allocatable :: exc, eyc, ezc
        type(Array3), allocatable :: cxc, cyc, czc, dely
    end type



contains 

    function init_cell_metrics(x, y, z, resolution) result(self)
        type(CellMetric3) :: self
        class(Array3), intent(in) :: x, y, z ! x y z 's ghost points are already set
        integer, intent(in) :: resolution(3)
        type(Vector3) :: a, b, c, d, e, f, g, h
        type(Vector3) :: u, v, w, xi, eta, zeta, px, py, pz
        real(real64) :: v1, v2, v3, v4, v5, vol
        integer :: i, j, k

        self%m_resolution = resolution
        self%sx = Array3(resolution(1), resolution(2), resolution(3))
        allocate (self%sy, self%sz, source=self%sx)

        allocate ( &
            self%ex, self%ey, self%ez, &
            self%cx, self%cy, self%cz, &
            self%sxc, self%syc, self%szc, &
            self%exc, self%eyc, self%ezc, &
            self%cxc, self%cyc, self%czc, &
            self%dely, self%aj, &
            source=self%sx)

        do concurrent(i=1:resolution(1), j=1:resolution(2), k=1:resolution(3)) local(a, b, c, d, e, f, g, h)
            block
                real(real64) :: xaa, xbb, xcc, xdd, xee, xff, xgg, xhh, &
                                yaa, ybb, ycc, ydd, yee, yff, ygg, yhh, &
                                zaa, zbb, zcc, zdd, zee, zff, zgg, zhh
                call calc_center_point(x%m_data, i, j, k, xaa, xbb, xcc, xdd, xee, xff, xgg, xhh)
                call calc_center_point(y%m_data, i, j, k, yaa, ybb, ycc, ydd, yee, yff, ygg, yhh)
                call calc_center_point(z%m_data, i, j, k, zaa, zbb, zcc, zdd, zee, zff, zgg, zhh)

                a = [xaa, yaa, zaa]
                b = [xbb, ybb, zbb]
                c = [xcc, ycc, zcc]
                d = [xdd, ydd, zdd]
                e = [xee, yee, zee]
                f = [xff, yff, zff]
                g = [xgg, ygg, zgg]
                h = [xhh, yhh, zhh]
            end block

            !---- 3-D Volume (Finite Volume Approach)

            v1 = (((f - b) .cross. (a - b))) .dot. (d - b) / 6.d0
            v2 = (((a - e) .cross. (f - e))) .dot. (g - e) / 6.d0
            v3 = (((g - c) .cross. (d - c))) .dot. (a - c) / 6.d0
            v4 = (((d - h) .cross. (g - h))) .dot. (f - h) / 6.d0
            v5 = (((a - d) .cross. (g - d))) .dot. (f - d) / 6.d0
            vol = (v1 + v2 + v3 + v4 + v5)

            self%aj%m_data(i, j, k) = 1.d0 / vol

            !---- 3-D Metric (Finite Difference Approach)
            xi = 0.25d0 * ((b - a) + (d - c) + (f - e) + (h - g)) ! s => same as (s2 - s1) / 1.d0
            eta = 0.25d0 * ((e - a) + (g - c) + (f - b) + (h - d)) ! e => same as (s2 - s1) / 1.d0
            zeta = 0.25d0 * ((c - a) + (d - b) + (h - f) + (g - e)) ! c => same as (s2 - s1) / 1.d0

            !---- 3-D Jacobian

            u = (1.d0 / vol) * (eta.cross.zeta)
            v = (1.d0 / vol) * (zeta.cross.xi)
            w = (1.d0 / vol) * (xi.cross.eta)

            self%sx%m_data(i, j, k) = u%x; self%ex%m_data(i, j, k) = v%x; self%cx%m_data(i, j, k) = w%x; 
            self%sy%m_data(i, j, k) = u%y; self%ey%m_data(i, j, k) = v%y; self%cy%m_data(i, j, k) = w%y; 
            self%sz%m_data(i, j, k) = u%z; self%ez%m_data(i, j, k) = v%z; self%cz%m_data(i, j, k) = w%z; 
        !!---- 3-D Mesh Cell Interface
            px = .5d0 * ((h - b) .cross. (d - f))
            py = .5d0 * ((h - e) .cross. (f - g))
            pz = .5d0 * ((h - c) .cross. (g - d))

            self%sxc%m_data(i, j, k) = px%x; self%exc%m_data(i, j, k) = py%x; self%cxc%m_data(i, j, k) = pz%x; 
            self%syc%m_data(i, j, k) = px%y; self%eyc%m_data(i, j, k) = py%y; self%cyc%m_data(i, j, k) = pz%y; 
            self%szc%m_data(i, j, k) = px%z; self%ezc%m_data(i, j, k) = py%z; self%czc%m_data(i, j, k) = pz%z; 
        end do

    end function

    pure subroutine calc_center_point(pt, i, j, k, a, b, c, d, e, f, g, h)
        real(real64), intent(in), dimension(:, :, :), allocatable :: pt
        integer, intent(in) :: i, j, k
        real(real64), intent(out) :: a, b, c, d, e, f, g, h
        integer :: ipp, jpp, kpp, imm, jmm, kmm
        ipp = i + 1; imm = i - 1
        jpp = j + 1; jmm = j - 1
        kpp = k + 1; kmm = k - 1
        !---- 3-D Mesh Points(Control Volume Points)
        !&<
        a = .125d0*(pt(imm, jmm, kmm) + pt(i  , jmm, kmm) &
                  + pt(i  , j  , kmm) + pt(imm, j  , kmm) &
                  + pt(imm, jmm, k  ) + pt(i  , jmm, k  ) &
                  + pt(i  , j  , k  ) + pt(imm, j  , k  ))

        b = .125d0*(pt(i   ,jmm, kmm) + pt(ipp, jmm, kmm) &
                  + pt(ipp ,j  , kmm) + pt(i  , j  , kmm) &
                  + pt(i   ,jmm, k  ) + pt(ipp, jmm, k  ) &
                  + pt(ipp ,j  , k  ) + pt(i  , j  , k  ))

        c = .125d0*(pt(imm ,jmm, k  ) + pt(i  , jmm, k  ) &
                  + pt(i   ,j  , k  ) + pt(imm, j  , k  ) &
                  + pt(imm ,jmm, kpp) + pt(i  , jmm, kpp) &
                  + pt(i   ,j  , kpp) + pt(imm, j  , kpp))

        d = .125d0*(pt(i   ,jmm, k  ) + pt(ipp, jmm, k  ) &
                  + pt(ipp ,j  , k  ) + pt(i  , j  , k  ) &
                  + pt(i   ,jmm, kpp) + pt(ipp, jmm, kpp) &
                  + pt(ipp ,j  , kpp) + pt(i  , j  , kpp))

        e = .125d0*(pt(imm ,j  , kmm) + pt(i  , j  , kmm) &
                  + pt(i   ,jpp, kmm) + pt(imm, jpp, kmm) &
                  + pt(imm ,j  , k  ) + pt(i  , j  , k  ) &
                  + pt(i   ,jpp, k  ) + pt(imm, jpp, k  ))

        f = .125d0*(pt(i   ,j  , kmm) + pt(ipp, j  , kmm) &
                  + pt(ipp ,jpp, kmm) + pt(i  , jpp, kmm) &
                  + pt(i   ,j  , k  ) + pt(ipp, j  , k  ) &
                  + pt(ipp ,jpp, k  ) + pt(i  , jpp, k  ))

        g = .125d0*(pt(imm ,j  , k  ) + pt(i  , j  , k  ) &
                  + pt(i   ,jpp, k  ) + pt(imm, jpp, k  ) &
                  + pt(imm ,j  , kpp) + pt(i  , j  , kpp) &
                  + pt(i   ,jpp, kpp) + pt(imm, jpp, kpp))

        h = .125d0*(pt(i   ,j  , k  ) + pt(ipp, j  , k  ) &
                  + pt(ipp ,jpp, k  ) + pt(i  , jpp, k  ) &
                  + pt(i   ,j  , kpp) + pt(ipp, j  , kpp) &
                  + pt(ipp ,jpp, kpp) + pt(i  , jpp, kpp))
        !&>
    end subroutine calc_center_point
end module