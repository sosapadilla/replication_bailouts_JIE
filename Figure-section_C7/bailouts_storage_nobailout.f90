! Version with "storage/savings" technology for the banker. 
! Newest addition: allow for these savings to be inpacted by the epsilon shocks.
! This is controlled by parameter: k_epsilon_frac.
module param

IMPLICIT NONE

INTEGER :: writeout = 1, simulonly = 0, maxiter = 100, runwelf = 0
DOUBLE PRECISION :: indicator_external = 0d+0, indicator_bailout_default = 0d+0
! indicator_bailout_default (0: no bailout during default | 1: bailout during default)

INTEGER :: b_num, k_num, y_num, eps_num, quad_num, nout, i_y_global, i_e_global, index_crisis_global, &
    i_b_global, i_default_global, i_b_next, iteration,  tax_num, transfer_num, epsilon_num, i_k_next, i_k_global, i_k_simu
PARAMETER (b_num= 50, k_num= 15, y_num =25, tax_num=25, transfer_num = 1, epsilon_num = 4)

double precision, dimension(b_num,k_num,y_num) ::  exp_v_vector, exp_w_vector, q_vector, expected_one_plus_r_tomorrow_from_repay
double precision, dimension(k_num,y_num) :: exp_v_default, exp_w_default, expected_one_plus_r_tomorrow_from_default
DOUBLE PRECISION :: b_grid(1:b_num), k_grid(1:k_num), y_grid(1:y_num), default_grid(1:2), &
    vector_v_global(y_num), y_initial, b_initial, b_global, tax_grid(1:tax_num), &
    transfer_grid(1:transfer_num), transfer_global, eshock(1:epsilon_num),&
    cdfe(1:epsilon_num), petran(1:epsilon_num), cum_petran(1:epsilon_num),&
    Erd(y_num), bc_grid(1:2), avg_welf_bk(b_num,k_num), k_initial

DOUBLE PRECISION :: sigma, b_inf, b_sup, y_inf, y_sup, eps_inf, eps_sup, std_y, pi_number, lambda,  &
    zero, width, prob_excl_end, pi, delta_bond, theta, tax_inf, tax_sup, kapital, mean_y, rw,     &
    transfer_inf, transfer_sup, epsmin, epsmax, bankcrisis_prob, A_worst_case, indicator_lumpsum,&
    damp_v, damp_q, k_inf, k_sup

    PARAMETER (sigma = 2d+0, pi_number = 3.1415926535897932d+0, lambda = 0d+0, zero = 0d+0, width = 1.5d+0,&
    prob_excl_end = 0.5d+0, delta_bond = 1.0d+0, theta=1d+0, kapital=1d+0, mean_y = 0d+0, epsmin = 1.0d+0, &
    epsmax = EXP(1.0d+0), bankcrisis_prob = 0.0181d+0, rw = 0.04d+0, damp_v= 0d+0, damp_q = 0d+0)

    ! beta = 0.81232233d+0, 
    ! Parameters for "SMM":
    DOUBLE PRECISION :: gov_spending = 0.14646447d+0, sig_e = 4.2570787d+0, T_max_frac = 0.0d+0
    DOUBLE PRECISION, PARAMETER :: rho = 0.8, std_eps = 0.0175
    ! Parameters that will be read from outside files:
    DOUBLE PRECISION :: k_epsilon_frac, AA, scale_mk, alpha_k, beta

    INTEGER, PARAMETER :: N_bita = 1, N_abar = 1, N_sigmae = 1, N_gov = 1, N_tmax = 1
    DOUBLE PRECISION, dimension (N_bita) :: bita_grid
    DOUBLE PRECISION, dimension (N_abar) :: abar_grid
    DOUBLE PRECISION, dimension (N_sigmae) :: sig_e_grid
    DOUBLE PRECISION, dimension (N_gov) :: gov_grid
    DOUBLE PRECISION, dimension (N_tmax) :: tmax_grid



    DOUBLE PRECISION ::beta_bank, ghh_indicator, omega, sigma_c, sigma_n, chi, alpha, r_min, &
    gamma_firm, m_NX, gamma_bank, rf_rate, coupon, mrate
    PARAMETER (beta_bank=0.96d+0, omega=2.5d+0, ghh_indicator=1d+0, sigma_c = 2d+0, &
    alpha=0.7d+0, sigma_n=2d+0, chi=0.01d+0, r_min=0.0d+0, gamma_firm=0.52d+0, m_NX = 0d+0, gamma_bank = 1d+0, &
    rf_rate = 1.0d+0/beta_bank-1.0d+0, coupon = rf_rate/(1.0d+0 + rf_rate), mrate = 1d+0)

  

    DOUBLE PRECISION :: tax_lp_and_default, tax_repay, deviation, dev_q

    DOUBLE PRECISION, DIMENSION(k_num, y_num, epsilon_num,2) :: w1_matrix, v1_matrix, vw1_matrix, r1_matrix, consumption1_matrix,&
        loan1_matrix, tax1_matrix, labor1_matrix, transfer1_matrix, guarantee1_matrix, cons_bank1_matrix

    DOUBLE PRECISION, DIMENSION(b_num, k_num, y_num, epsilon_num,2) :: w0_matrix, v0_matrix, vw0_matrix, r0_matrix,    &
        consumption0_matrix, loan0_matrix, tax0_matrix, labor0_matrix, transfer0_matrix, guarantee0_matrix,     &
        cons_bank0_matrix

    INTEGER, DIMENSION(b_num, k_num, y_num, epsilon_num,2) :: b_next0_matrix, k_next_matrix



    INTEGER, DIMENSION(b_num, k_num, y_num, epsilon_num) ::  default_decision, default_decision_new
    DOUBLE PRECISION, DIMENSION(b_num, k_num, y_num, epsilon_num) ::value_matrix, value_repay_matrix, value_default_matrix, &
    value_hh_matrix, value_hh_repay_matrix, value_hh_default_matrix, default_matrix,    &
    value_bank_matrix, value_bank_repay_matrix, value_bank_default_matrix, output_mat_no_transfer, output_mat_opt_transfer
    INTEGER, DIMENSION(b_num, k_num, y_num, epsilon_num,2) :: b_next_matrix
    DOUBLE PRECISION, DIMENSION(b_num, k_num, y_num,epsilon_num,2) :: v_matrix, w_matrix, vw_matrix,&
    b0_next_matrix, b1_next_matrix, q_paid_matrix, q_matrix_nodef, r_matrix, n_matrix, transfer_matrix, &
    tax_matrix, loan_matrix, guarantee_matrix
    DOUBLE PRECISION, DIMENSION(b_num,k_num, y_num,epsilon_num,2) :: consumption_matrix, cons_bank_matrix
    DOUBLE PRECISION, DIMENSION(y_num, y_num) :: trans_matrix, cdf_matrix
    DOUBLE PRECISION, DIMENSION (b_num, k_num, y_num, epsilon_num) :: welfares

    DOUBLE PRECISION :: avg_loan_to_y, avg_transfer_to_y, def_prob_unconditional, def_prob_conditional, &
    avg_b_to_y, avg_g_to_y, avg_std_log_y, T_max, T_max_grid(1:k_num), avg_welf, avg_spread, avg_std_spread,&
    avg_corr_spread_y, avg_rr, avg_log_y, avg_y, avg_c, avg_spread_bc, avg_std_spread_bc, avg_b, &
    def_banking_crisis, avg_ytm_bond, avg_ytm_free, avg_spread_alt, avg_k, avg_k_to_y,&
    avg_exposure_1, avg_exposure_2, avg_sum_of_A, avg_frac_endog_A, avg_M_k, avg_k_to_assets, &
    avg_loan_to_y_def, avg_loan, avg_loan_def, loan_drop, avg_rr_BC, avg_rr_DEF, avg_rr_BC_DEF

CONTAINS 

DOUBLE PRECISION FUNCTION COVARIANCE(A, B, N, mask)
! Computes the covariance of two series A and B of length N.

  IMPLICIT NONE

  INTEGER, INTENT(IN)                   :: N
  DOUBLE PRECISION, DIMENSION(N), INTENT(IN)    :: A
  DOUBLE PRECISION, DIMENSION(N), INTENT(IN)    :: B
  LOGICAL, DIMENSION(N),INTENT(IN),OPTIONAL          :: mask
!   DOUBLE PRECISION                            :: COVARIANCE
  
  DOUBLE PRECISION, DIMENSION(N)                :: Adev
  DOUBLE PRECISION, DIMENSION(N)                :: Bdev
  DOUBLE PRECISION, DIMENSION(N)                :: AdevBdev

  DOUBLE PRECISION                              :: xmn
  DOUBLE PRECISION                              :: ymn
  
  IF (PRESENT(mask)) THEN
      
      ! xmn = SUM(A,mask) / REAL(COUNT(mask),DP)
      ! ymn = SUM(B,mask) / REAL(COUNT(mask),DP)

      xmn = SUM(A,mask) / REAL(COUNT(mask))
      ymn = SUM(B,mask) / REAL(COUNT(mask))

      Adev = A - xmn
      Bdev = B - ymn

      AdevBdev = Adev * Bdev

      ! COVARIANCE = SUM(AdevBdev,mask)/REAL(COUNT(mask)-1,DP)
      COVARIANCE = SUM(AdevBdev,mask)/REAL(COUNT(mask)-1)

  ELSE

      ! xmn = SUM(A) / REAL(N,DP)
      ! ymn = SUM(B) / REAL(N,DP)

      xmn = SUM(A) / REAL(N)
      ymn = SUM(B) / REAL(N)

      Adev = A - xmn
      Bdev = B - ymn

      AdevBdev = Adev * Bdev

      ! COVARIANCE = SUM(AdevBdev)/REAL(N-1,DP)
      COVARIANCE = SUM(AdevBdev)/REAL(N-1)

  END IF
  
END FUNCTION COVARIANCE

FUNCTION AVG(A,N,mask)

    ! Computes the mean of a vector A of length
    ! N. 

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(N), INTENT(IN)  :: A 
    INTEGER, INTENT(IN)                         :: N
    LOGICAL,  DIMENSION(N), INTENT(IN),OPTIONAL :: mask
    DOUBLE PRECISION                            :: AVG
  
    IF (PRESENT(mask)) THEN
        AVG = SUM(A,mask) / DBLE(COUNT(mask))
    ELSE
        AVG = SUM(A) / DBLE(N)
    END IF
  
END FUNCTION AVG
    
INTEGER FUNCTION multpl(i,m)
IMPLICIT NONE
INTEGER, INTENT(IN) :: i,m
REAL*8, PARAMETER :: toler = 1.0D-12
REAL*8 :: remain

remain = DBLE(REAL(i))/DBLE(REAL(m))-i/m

IF (remain <= toler) then
    multpl = 1
ELSE
    multpl = 0
ENDIF

RETURN
END

SUBROUTINE creategrid(grid, siz, grid_min, grid_pivot, grid_max, curvature, adj_pivot)

    
    ! INPUT & OUTPUT
    INTEGER, INTENT(IN)                     :: siz
    DOUBLE PRECISION, INTENT(IN)            :: grid_min, grid_max, grid_pivot
    DOUBLE PRECISION, INTENT(IN)            :: curvature
    DOUBLE PRECISION, INTENT(IN), OPTIONAL  :: adj_pivot
    
    DOUBLE PRECISION, DIMENSION(SIZ), INTENT(INOUT) :: grid
    
    ! LOCAL VARIABLES
    INTEGER                                 :: i, n_med
    DOUBLE PRECISION                        :: adj_pvt
     
    ! PROGRAM
    IF (PRESENT(adj_pivot)) THEN
        adj_pvt = adj_pivot
    ELSE
        adj_pvt = 1.0d+0
    END IF
    
    ! initialization
    grid        =   grid_min
    
    IF_SINGLE: IF (siz .EQ. 1) THEN
        ! IF size of the grid is one
        ! THEN set to lower bound
        grid        =   grid_min
    ELSE

        n_med       = MAX(1,CEILING(adj_pvt*siz*((grid_pivot - grid_min)/ &
                            (grid_max - grid_min))))

        IF (n_med .GT. 1) THEN
            FORALL (i=1:n_med) grid(i) = DBLE(i-1)/DBLE(n_med-1)      ! [0,1] grid evenly split
            grid(1:n_med)    = 1.0d+0 - grid(1:n_med)                ! [1,0] grid evenly split
            grid(1:n_med)    = 1.0d+0 - grid(1:n_med)**curvature        ! [0,1] grid clustered at 1
            FORALL (i=1:n_med) grid(i) = grid_min + &
            (grid_pivot-grid_min)*grid(i)   ! grid_min to grid_pivot, clustered
        END IF

        IF (n_med .LT. siz) THEN
            FORALL (i=n_med:siz) grid(i) = DBLE(i-n_med)/DBLE(siz-n_med)   ! even grid between 0 and 1
            grid(n_med:siz)    = grid(n_med:siz)**curvature            ! [0,1] grid clustered at 0
            FORALL (i=n_med:siz) grid(i) = grid_pivot + &
            (grid_max-grid_pivot)*grid(i)   ! grid_min to grid_pivot, clustered
        END IF

    END IF IF_SINGLE
                                                        
END SUBROUTINE creategrid

END MODULE param

FUNCTION interp1(X,Y,siz,xi)
!! Interpolates to find the values of the underlying function Y at the points in the vector or array xi.
!! X and Y must be one-dimensional arrays with the same dimension siz
!! xi must be a scalar
!! IF xi exceeds the bounds of X, i.e. requiring extrapolation, THEN Yi is set to the closest point
    
! INPUT & OUTPUT
INTEGER, INTENT(IN)                     :: siz
DOUBLE PRECISION, DIMENSION(siz), INTENT(IN)    :: X, Y
DOUBLE PRECISION, INTENT(IN)                    :: xi
DOUBLE PRECISION                               :: interp1
INTEGER                                    :: ind
    
! PROGRAM
ind = MINLOC( ABS( xi - X ) , 1 )
outer: IF ( xi >= X(ind) ) THEN
    inner1: IF  ( ind == siz ) THEN
        interp1 = Y(ind)
    ELSE
        interp1 =    &
            ( Y(ind+1)*(xi-X(ind)) + Y(ind)*(X(ind+1)-xi) )    &
            /( X(ind+1) - X(ind) )
    END IF inner1
ELSE ! IF ( xi < X(ind) )
    inner2: IF  ( ind == 1 ) THEN
        interp1 = Y(ind)
    ELSE
        interp1 =    &
            ( Y(ind)*(xi-X(ind-1)) + Y(ind-1)*(X(ind)-xi) )    &
            /( X(ind) - X(ind-1) )
    END IF inner2
END IF outer
    
END FUNCTION interp1
    
DOUBLE PRECISION FUNCTION u_fun(c,n)
USE param
IMPLICIT NONE
DOUBLE PRECISION, INTENT (IN) :: c,n
DOUBLE PRECISION :: c_min, cons, u_fun_c, u_fun_n, argument

c_min = 1d-10
cons = MAX(c,c_min)



IF (c <= 0d+0.or.n<=0d+0.or.n>1d+0) THEN
   u_fun = -1000d+0
ELSE
   IF (ghh_indicator>0.5d+0) THEN
        argument = c-n**(omega)/omega
        IF (argument<=0d+0) THEN
            u_fun = -1000d+0
        ELSE    
            IF (sigma_c == 1d+0) THEN
                u_fun = LOG(argument)
            ELSE
                u_fun = (argument**(1d+0-sigma_c) ) / (1d+0-sigma_c)
            END IF
        ENDIF
   ELSE
      IF (sigma_c == 1d+0) THEN
         u_fun_c = LOG(cons)
      ELSE
         u_fun_c = (c**(1d+0-sigma_c) ) / (1d+0-sigma_c)
      END IF

      IF (sigma_n == 1d+0) THEN
         u_fun_n = LOG(1d+0-n)
      ELSE
         u_fun_n = ((1d+0 - n)**(1d+0-sigma_n) ) / (1d+0-sigma_n)
      END IF
      u_fun=u_fun_c + chi * u_fun_n

   END IF

END IF

END function u_fun     

!SPECIFY GRID VALUES
SUBROUTINE compute_grid
USE param
IMPLICIT NONE
INTEGER :: i, j, iter, idx
INTEGER, parameter :: Nb_low = 5, Nb_high = 20, Nb_mid = b_num - Nb_low - Nb_high
DOUBLE PRECISION :: delta_y, prob_mass, y_pointer, cdf_right, cdf_left, qq, pdf_tirar,&
first_mom, second_mom, std_pareto, b_int1, b_int2, grid_low(Nb_low), grid_mid(Nb_mid), grid_high(Nb_high), step, &
k_ss_approx
DOUBLE PRECISION, DIMENSION(y_num)  ::  Erdold 
double precision, external :: m_fun 

! Nb_low = 10
! Nb_high = 20

std_y = std_eps/ SQRT(1d+0 - rho**2d+0)

y_inf = mean_y - 4d+0*std_y
y_sup = mean_y + 4d+0*std_y
delta_y = 4d+0*std_y / (y_num - 1d+0)

tax_inf=0.0d+0
tax_sup=0.99d+0

transfer_inf=0.0d+0
transfer_sup=1.0d+0

b_inf = 0.00d+0
b_sup = 0.8d+0
b_int1 = 0.10d+0
b_int2 = 0.40d+0

k_inf = 0.00d+0
k_ss_approx = (scale_mk * beta_bank * alpha_k) ** (1d+0 / (1d+0 - alpha_k))
k_sup = min(max(k_ss_approx *2d+0, 0.05d+0), 0.2d+0)



IF (writeout.EQ.1) THEN
    OPEN (10, FILE='graphs_b_grid_dss.txt',STATUS='replace')
    OPEN (11, FILE='graphs_y_grid_dss.txt',STATUS='replace')
    OPEN (12, FILE='graphs_tax_grid_dss.txt',STATUS='replace')
    OPEN (13, FILE='graphs_bounds_dss.txt',STATUS='replace')
    OPEN (14, FILE='graphs_delta_bond.txt',STATUS='replace')
    OPEN (15, FILE='graphs_transfer_grid_dss.txt',STATUS='replace')
    OPEN (16, FILE='graphs_k_grid_dss.txt',STATUS='replace')

    WRITE(13, '(F15.11, X, F15.11)') b_inf, b_sup
    WRITE(13, '(F15.11, X, F15.11)') k_inf, k_sup
    WRITE(13, '(F15.11, X, F15.11)') y_inf, y_sup    
    
    WRITE(14, '(F15.11, X, F15.11)') delta_bond
END IF



DO i=1,b_num
   b_grid(i) = b_inf + (b_sup - b_inf)*(i-1)/(b_num - 1)
END DO

DO i=1,k_num
   k_grid(i) = k_inf + (k_sup - k_inf)*(i-1)/(k_num - 1)
END DO

IF (writeout.EQ.1) THEN
    DO i=1,b_num
        WRITE(10, '(F12.8)') b_grid(i)
    END DO

    DO i=1,k_num
        WRITE(16, '(F12.8)') k_grid(i)
    END DO
ENDIF

DO i=1,y_num
   y_grid(i) = y_inf + (y_sup - y_inf) * (i-1) / (y_num - 1)
    IF (writeout.EQ.1) THEN
        WRITE(11, '(F12.8)') y_grid(i)
    ENDIF
END DO

DO i=1,tax_num
   tax_grid(i) = tax_inf + (tax_sup - tax_inf) * (i-1) / (tax_num - 1)
    IF (writeout.EQ.1) THEN
        WRITE(12, '(F12.8)') tax_grid(i)
    ENDIF
END DO

IF (transfer_num>1) THEN
    DO i=1,transfer_num
        transfer_grid(i) = transfer_inf + (transfer_sup - transfer_inf) * (i-1) / (transfer_num - 1)
        IF (writeout.EQ.1) THEN
            WRITE(15, '(F12.8)') transfer_grid(i)
        ENDIF
    END DO
ELSE
    transfer_grid = transfer_inf
    IF (writeout.EQ.1) THEN
        WRITE(15, '(F12.8)') transfer_grid
    ENDIF
ENDIF


DO i=1,y_num
   y_pointer = (y_sup + delta_y - (1d+0-rho)*mean_y - rho*y_grid(i))/ std_eps
   CALL nprob(y_pointer, cdf_right, qq, pdf_tirar)

   y_pointer = (y_inf - delta_y - (1d+0-rho)*mean_y - rho*y_grid(i))/ std_eps
   CALL nprob(y_pointer, cdf_left, qq, pdf_tirar)

   prob_mass = cdf_right - cdf_left

   DO j=1,y_num

      y_pointer = (y_grid(j) + delta_y - (1d+0-rho)*mean_y - rho*y_grid(i))/ std_eps
      CALL nprob(y_pointer, cdf_right, qq, pdf_tirar)

      y_pointer = (y_grid(j) - delta_y - (1d+0-rho)*mean_y - rho*y_grid(i))/ std_eps
      CALL nprob(y_pointer, cdf_left, qq, pdf_tirar)

      trans_matrix(i,j) = (cdf_right - cdf_left )/prob_mass

   END DO

END DO


DO i=1,y_num
   cdf_matrix(i,1) = trans_matrix(i,1)
   DO j=2,y_num
      cdf_matrix(i,j) = cdf_matrix(i,j-1) + trans_matrix(i,j)
   END DO
END DO

IF (writeout.EQ.1) THEN
    CLOSE(10)
    CLOSE(11)
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)
    CLOSE(15)
    CLOSE(16)
    OPEN (17, FILE='graphs_trans_matrix_dss.txt',STATUS='replace')
    WRITE(17, '(F12.8)') trans_matrix
    CLOSE(17)

    OPEN (18, FILE='graphs_cdf_matrix_dss.txt',STATUS='replace')
    WRITE(18, '(F12.8)') cdf_matrix
    CLOSE(18)
END IF


    
Erdold = 1d+0/DBLE(y_num)
iter = 0
    
ERD_LOOP: DO
    iter = iter + 1
    Erd = MATMUL(Erdold,trans_matrix)
    IF (MAXVAL(ABS(Erd-Erdold))/DBLE(y_num) < 1.0d-8) THEN
        EXIT ERD_LOOP
    END IF
    Erdold = Erd
END DO ERD_LOOP

default_grid(1) = 0d+0
default_grid(2) = 1d+0

bc_grid(1) = 0d+0
bc_grid(2) = 1d+0


!====== HERE CREATE THE EPSILON_SHOCK_GRID ======

! Generate epsilon shocks (Bounded Pareto Distribution)
CALL mom_pareto(epsmin, epsmax, sig_e, 1.0d+0, first_mom)
CALL mom_pareto(epsmin, epsmax, sig_e, 2.0d+0, second_mom)
std_pareto = (second_mom - (first_mom**2))**0.5


ELOOP: DO i =1,epsilon_num-1
      eshock(i) = 1.0d+0 + (i-1)*std_pareto
      CALL pareto_cdf(epsmin, epsmax, sig_e, eshock(i) + 0.5d+0*std_pareto, cdfe(i))
      IF (i.EQ.1) THEN
         petran(i) = cdfe(i)
      ELSE
         petran(i) = cdfe(i) - cdfe(i-1)
      ENDIF
      END DO ELOOP
      eshock(epsilon_num) = MIN(epsmax, 1d+0 + (epsilon_num-1)*std_pareto)
      petran(epsilon_num) = 1d+0 - cdfe(epsilon_num-1)

   ! PRINT *, "e shocks: ", eshock
   PRINT *, "pe tran: ", petran

   eshock = LOG(eshock)
   PRINT *, "transformed e shocks: ", eshock
   PRINT *, "E(e shock): ", SUM(eshock*petran)
   !std_dev_eshock = (COVARIANCE(eshock,eshock,epsilon_num))**0.5d+0

   ! PRINT *, "std_dev of transformed grid", std_dev_eshock
    
    pi = bankcrisis_prob/(1d+0-cdfe(1))
    PRINT *, "pi shock", pi

    cum_petran = 0d+0

    cum_petran(1) = petran(1)
       DO j=2,epsilon_num
           cum_petran(j) = cum_petran(j-1) + petran(j)
       END DO
    cum_petran(epsilon_num) = 1.0d+0
    
    ! T_max = AA*eshock(epsilon_num)*T_max_frac
    ! PRINT *, 'T_max:', T_max
    do i = 1, k_num
        T_max_grid(i) = (AA + m_fun(k_grid(i))*k_epsilon_frac)*eshock(epsilon_num)*T_max_frac
        PRINT *, 'T_max_grid(', i, ') = ', T_max_grid(i)        
    end do        

    IF (writeout.EQ.1) THEN
        OPEN (16, FILE='graphs_epsilon_grid_dss.txt',STATUS='replace')
        DO i=1,epsilon_num
           WRITE(16, '(F12.8)') eshock(i)
        END DO
        CLOSE(16)

        OPEN (16, FILE='graphs_cum_petran_dss.txt',STATUS='replace')
        WRITE(16, '(F12.8)') cum_petran
        CLOSE(16)

        OPEN (16, FILE='graphs_petran_dss.txt',STATUS='replace')
        WRITE(16, '(F12.8)') petran
        CLOSE(16)
    END IF
    
END SUBROUTINE compute_grid


DOUBLE PRECISION function y_fun(n)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT (IN) :: n

    y_fun=EXP(y_initial)*n**(alpha)*kapital**(1d+0-alpha)

END function y_fun

! m_fun is the banker's storage technology. 
DOUBLE PRECISION function m_fun(kk)
    USE param
    implicit none
    DOUBLE PRECISION, INTENT (IN) :: kk
    m_fun = scale_mk * kk**(alpha_k)
end function m_fun

! First derivative of the m_fun
DOUBLE PRECISION function m_prime_fun(kk)
    USE param
    implicit none
    DOUBLE PRECISION, INTENT (IN) :: kk
    m_prime_fun = scale_mk * alpha_k * kk**(alpha_k - 1d+0)
end function m_prime_fun

DOUBLE PRECISION function loan_supply_fun(b,kk)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT (IN) :: b,kk
    DOUBLE PRECISION :: aux, d, m_after_hit
    DOUBLE PRECISION, EXTERNAL :: m_fun

    d = default_grid(i_default_global)
    m_after_hit = m_fun(kk) * (1d+0 - eshock(i_e_global) * k_epsilon_frac)

    if (indicator_bailout_default > 0.5d+0) then    
        aux = d*(A_worst_case + m_after_hit + transfer_global) + &
            (1d+0 - d) * (b *(mrate + (1.0d+0 - mrate) * (coupon + q_vector(i_b_next, i_k_next, i_y_global))) +&
            A_worst_case + transfer_global + m_after_hit)         
    else
        aux = d*(A_worst_case + m_after_hit) + &
            (1.0d+0 - d) * (b *(mrate + (1.0d+0 - mrate) * (coupon + q_vector(i_b_next, i_k_next, i_y_global))) +&
            A_worst_case + transfer_global + m_after_hit)         
    end if

    loan_supply_fun = gamma_bank * aux

end function loan_supply_fun

DOUBLE PRECISION function r_fun(n)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT (IN) :: n
    DOUBLE PRECISION :: aux, denominator
    DOUBLE PRECISION, EXTERNAL :: y_fun, loan_supply_fun

    denominator = loan_supply_fun(b_initial, k_initial)
    denominator = MAX(denominator, 1d-10) ! avoid division by zero
    aux  = (alpha*y_fun(n))/denominator -1d+0/gamma_firm

    r_fun = MAX (aux,r_min)

END function r_fun

SUBROUTINE ps_lp_and_default(consumption,labor,loan, rate)
USE param
IMPLICIT NONE
DOUBLE PRECISION :: consumption, labor, profit_firm, rr, loan, rate, d, bc, &
exit_flag, tax_opt, transfer_effective, wage_bill
DOUBLE PRECISION, EXTERNAL :: r_fun, y_fun, loan_supply_fun
EXTERNAL :: solve_tax_lp_and_default

d  = default_grid(i_default_global)
bc = bc_grid(index_crisis_global)

! This is what matters for the GBC:
    if (indicator_bailout_default > 0.5d+0) then    
        transfer_effective = (1d+0 - bc) *  0d+0 + bc * transfer_global 
    else
        transfer_effective = (1d+0 - d) * ((1d+0 - bc) *  0d+0 + bc * transfer_global) + d * 0d+0 
    endif


    loan = loan_supply_fun(b_initial, k_initial)
    wage_bill = loan / gamma_firm

    tax_lp_and_default = (gov_spending+transfer_effective+(1d+0-d)*b_initial*(mrate + (1d+0-mrate)*coupon))/wage_bill


    labor = (wage_bill - (gov_spending+transfer_effective+(1d+0-d)*b_initial*(mrate + (1d+0-mrate)*coupon)))**(1d+0/omega)

    profit_firm = (1d+0-alpha)* y_fun(labor)
    consumption = labor**(omega) + profit_firm - m_NX

    rr = r_fun(labor)

    IF (rr==r_min) THEN

        CALL solve_tax_lp_and_default(tax_opt, exit_flag)

        IF (exit_flag<0.5d+0) THEN
            tax_lp_and_default=tax_opt
            labor=(EXP(y_initial)*alpha*kapital**(1d+0-alpha)*&
                (1d+0-tax_lp_and_default)/ (1d+0+gamma_firm*r_min))**(1d+0/  (omega-alpha))
            wage_bill= labor**(omega)/(1d+0-tax_lp_and_default)
            loan = gamma_firm*wage_bill
            profit_firm=y_fun(labor)-(1d+0 + gamma_firm*r_min)*wage_bill
            consumption = labor**(omega) + profit_firm - m_NX
        ELSE
            !exit flag is 1, THEN something is not right--
            tax_lp_and_default = tax_opt
            consumption = -10d+0 ! this is to never choose this point
        END IF
    END IF
    rate = rr
    ! END IF

END SUBROUTINE ps_lp_and_default


SUBROUTINE ps_repay_full(consumption,labor,loan, rate)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION :: consumption, labor, profit_firm, rr, loan, rate, d, bc, &
    exit_flag, tax_opt, transfer_effective, wage_bill, debt_part, loan_supply
    DOUBLE PRECISION, EXTERNAL :: r_fun, y_fun, loan_supply_fun
    EXTERNAL :: solve_tax_full

    d  = default_grid(i_default_global)
    bc = bc_grid(index_crisis_global)

    ! This is what matters for the GBC:
    transfer_effective = (1d+0 - d) * ((1d+0 - bc) *  0d+0 + bc * transfer_global)  + d * 0d+0 

   
    
    loan = loan_supply_fun(b_initial, k_initial)
    wage_bill = loan / gamma_firm

    debt_part = b_initial*(mrate + (1d+0-mrate)*coupon) -&
        (b_grid(i_b_next)-(1d+0-mrate)*b_initial) * q_vector(i_b_next, i_k_next, i_y_global)

    tax_repay= (gov_spending+ transfer_effective + debt_part)/wage_bill
    labor = ((1d+0 - tax_repay) * wage_bill)**(1d+0/omega)
    profit_firm = (1d+0-alpha)* y_fun(labor)
    consumption = labor**(omega) + profit_firm - m_NX

    rr = r_fun(labor)

    IF (rr==r_min) THEN

        CALL solve_tax_full(tax_opt, exit_flag)

        IF (exit_flag<0.5d+0) THEN
            tax_repay = tax_opt
            labor =(EXP(y_initial)*alpha*kapital**(1d+0-alpha)*(1d+0-tax_repay)/(1d+0 +gamma_firm*r_min))**(1d+0/(omega-alpha))
            wage_bill= labor**(omega)/(1d+0-tax_repay)
            loan = gamma_firm*wage_bill
            profit_firm=y_fun(labor)-(1d+0 + gamma_firm*r_min)*wage_bill
            consumption = labor**(omega) + profit_firm - m_NX
        ELSE

            
            tax_repay=tax_opt
            consumption = -10d+0
        END IF

    END IF
    rate=rr
 

END SUBROUTINE ps_repay_full


DOUBLE PRECISION function q_fun(i_b, i_kk, i_y)
    USE param
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: i_b, i_kk, i_y
    DOUBLE PRECISION :: y_next, m_next, exp_m, exp_q, prob_crisis(2), exp_inner
    INTEGER :: i_y_next, i_e_next, i_crisis_next

    prob_crisis(1)=1d+0-pi
    prob_crisis(2)=pi
    
    exp_m = 0d+0
    m_next = beta_bank

    DO i_e_next = 1, epsilon_num
        DO i_y_next= 1 , y_num
            y_next = y_grid(i_y_next)
            exp_inner = 0d+0
            DO i_crisis_next = 1,2
                exp_inner = exp_inner + & 
                ((1d+0 + r_matrix(i_b, i_kk, i_y_next, i_e_next, i_crisis_next))*(mrate + (1d+0-mrate)*(coupon + &
                q_matrix_nodef(b_next0_matrix(i_b, i_kk, i_y_next,i_e_next,i_crisis_next), &
                k_next_matrix(i_b, i_kk, i_y_next,i_e_next,i_crisis_next),i_y_next,i_e_next, i_crisis_next)))) * &
                prob_crisis(i_crisis_next)
            end do
            exp_m = exp_m +  (1d+0 - default_grid(default_decision(i_b, i_kk, i_y_next, i_e_next)))* &
                exp_inner * petran(i_e_next) * trans_matrix(i_y, i_y_next)
        END DO
    END DO

    q_fun = m_next * exp_m

END function q_fun

! This function computes the expected value of the next period's (1+loan rate), given the current choices (i_bb, i_kk) and the current state, i_y.
DOUBLE PRECISION function rloan_next(i_bb, i_kk, i_y)
    USE param
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: i_bb, i_kk, i_y
    DOUBLE PRECISION :: y_next, m_next, exp_m, exp_q, prob_crisis(2)
    INTEGER :: i_y_next, i_e_next, i_crisis_next

    prob_crisis(1) = 1d+0-pi
    prob_crisis(2) = pi

    exp_m = 0d+0

    DO i_crisis_next = 1,2
        DO i_e_next = 1,epsilon_num
            DO i_y_next=1,y_num
                exp_m = exp_m +(1d+0 + r_matrix(i_bb, i_kk,  i_y_next, i_e_next,i_crisis_next))*&
                trans_matrix(i_y, i_y_next)*petran(i_e_next)*prob_crisis(i_crisis_next)
        
            END DO
        END DO
    END DO

    rloan_next = exp_m

END function rloan_next


DOUBLE PRECISION function expected_one_plus_r_fun(i_b, i_kk, i_y)
    USE param
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: i_b, i_kk, i_y
    DOUBLE PRECISION :: y_next, m_next, exp_m_repay, exp_m_default, exp_q, prob_crisis(2),&
    exp_inner, def_today, exp_m_exlc_continues, exp_m_exlc_ends, scale_factor_k(epsilon_num, 2)
    INTEGER :: i_y_next, i_e_next, i_crisis_next

    def_today = default_grid(i_default_global)
    
    prob_crisis(1)=1d+0-pi
    prob_crisis(2)=pi

    ! Compute the scale_factor_k. If k is not affected by bc, this is always 1.
    DO i_e_next = 1, epsilon_num
        DO i_crisis_next = 1,2
            scale_factor_k(i_e_next, i_crisis_next) =  ((1d+0 - bc_grid(i_crisis_next)) +&
            bc_grid(i_crisis_next) * (1d+0 - eshock(i_e_next) * k_epsilon_frac))
        end do
    end do
    
    ! Repayment part:
    exp_m_repay = 0d+0
    DO i_e_next = 1, epsilon_num
        DO i_y_next= 1 , y_num
            y_next = y_grid(i_y_next)
            exp_inner = 0d+0
            DO i_crisis_next = 1,2
                exp_inner = exp_inner + & 
                (1d+0 + r0_matrix(i_b, i_kk, i_y_next, i_e_next, i_crisis_next))* &
                scale_factor_k(i_e_next, i_crisis_next) * prob_crisis(i_crisis_next)
            end do
            exp_m_repay = exp_m_repay +  (1d+0 - default_grid(default_decision(i_b, i_kk, i_y_next, i_e_next)))* &
                exp_inner * petran(i_e_next) * trans_matrix(i_y, i_y_next)
        END DO
    END DO

    ! Default part:
    exp_m_default = 0d+0
    DO i_e_next = 1, epsilon_num
        DO i_y_next= 1 , y_num
            y_next = y_grid(i_y_next)
            exp_inner = 0d+0
            DO i_crisis_next = 1,2
                exp_inner = exp_inner + &
                (1d+0 + r1_matrix(i_kk, i_y_next, i_e_next, i_crisis_next)) * &
                 scale_factor_k(i_e_next, i_crisis_next) * prob_crisis(i_crisis_next)
            end do
            exp_m_default = exp_m_default +  default_grid(default_decision(i_b, i_kk, i_y_next, i_e_next))* &
            exp_inner * petran(i_e_next) * trans_matrix(i_y, i_y_next)
        END DO
    END DO

    ! If excluded today:

    exp_m_exlc_continues = 0d+0
    DO i_e_next = 1, epsilon_num
        DO i_y_next= 1 , y_num
            y_next = y_grid(i_y_next)
            exp_inner = 0d+0
            DO i_crisis_next = 1,2                                
                exp_inner = exp_inner + & 
                (1d+0 + r1_matrix(i_kk, i_y_next, i_e_next, i_crisis_next)) *&
                scale_factor_k(i_e_next, i_crisis_next) * prob_crisis(i_crisis_next)
            end do
            exp_m_exlc_continues = exp_m_exlc_continues + exp_inner * petran(i_e_next) * trans_matrix(i_y, i_y_next)
        END DO
    END DO

    exp_m_exlc_ends = 0d+0
    DO i_e_next = 1, epsilon_num
        DO i_y_next= 1 , y_num
            y_next = y_grid(i_y_next)
            exp_inner = 0d+0
            DO i_crisis_next = 1,2
                exp_inner = exp_inner + & 
                (1d+0 + r0_matrix(i_b, i_kk, i_y_next, i_e_next, i_crisis_next)) *&
                scale_factor_k(i_e_next, i_crisis_next) * prob_crisis(i_crisis_next)
            end do
            exp_m_exlc_ends = exp_m_exlc_ends + exp_inner * petran(i_e_next) * trans_matrix(i_y, i_y_next)
        END DO
    END DO

    ! Expected value of (1 + r) for the next period:    
    
    expected_one_plus_r_fun =(1d+0-def_today)* (exp_m_repay + exp_m_default) + &
        def_today * (exp_m_exlc_ends * prob_excl_end + exp_m_exlc_continues * (1d+0-prob_excl_end))

END function expected_one_plus_r_fun




!Compute the objective function in the Bellman equation
DOUBLE PRECISION FUNCTION objective_function(consumption,labor)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: consumption,labor
    DOUBLE PRECISION :: exp_v_next
    DOUBLE PRECISION, EXTERNAL :: u_fun

    exp_v_next = exp_v_vector(i_b_next, i_k_next, i_y_global)

    objective_function = u_fun(consumption,labor) + beta * exp_v_next

END FUNCTION objective_function


!Compute the objective function in the Belman equation for the 'Exclusion Case' for the Household
DOUBLE PRECISION function objective_excl(cons_def, n_def)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: cons_def, n_def
    DOUBLE PRECISION :: exp_v_next, exp_v_next_excl
    DOUBLE PRECISION, EXTERNAL :: u_fun

    exp_v_next = exp_v_vector(i_b_next, i_k_next, i_y_global)
    exp_v_next_excl = exp_v_default(i_k_next, i_y_global)

    objective_excl = u_fun(cons_def, n_def) + beta *(prob_excl_end* exp_v_next + &
        (1d+0-prob_excl_end)*exp_v_next_excl)

END FUNCTION objective_excl


!Compute The Objective Function of the Banker
DOUBLE PRECISION function obj_banker(cons_banker)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: cons_banker
    DOUBLE PRECISION :: exp_w_next

    exp_w_next = exp_w_vector(i_b_next, i_k_next, i_y_global)
    obj_banker = cons_banker + beta_bank * exp_w_next

END FUNCTION obj_banker


!Compute the objective function in the Belman equation for the 'Exclusion Case'
DOUBLE PRECISION function obj_banker_excl(cons_banker)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: cons_banker
    DOUBLE PRECISION :: exp_w_next, exp_w_next_excl

    exp_w_next = exp_w_vector(i_b_next, i_k_next, i_y_global)
    exp_w_next_excl = exp_w_default(i_k_next, i_y_global)

    obj_banker_excl = cons_banker + beta_bank *(prob_excl_end* exp_w_next + (1d+0-prob_excl_end)*exp_w_next_excl)

END FUNCTION obj_banker_excl


SUBROUTINE optimize_lp(index_opt, cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, labor_bc, rate_bc,&
     tax_bc, ik_no_bc, ik_bc)
    USE param
    IMPLICIT NONE
    INTEGER, INTENT (OUT) :: index_opt,ik_no_bc, ik_bc
    DOUBLE PRECISION, INTENT (OUT) :: cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, labor_bc, rate_bc, tax_bc
    INTEGER :: index_vector(1),i, j, k_next_vector(transfer_num,2), index_k
    DOUBLE PRECISION :: vector(transfer_num), welfare(transfer_num,2) ,cc,nn, &
    v_vector(transfer_num,2), w_vector(transfer_num,2),ll,rr, cons_banker, &
    cons_vector(transfer_num,2), labor_vector(transfer_num,2), rate_vector(transfer_num,2), tax_vector(transfer_num,2), &
    transfer_effective, m_effective, total_damage
    DOUBLE PRECISION, EXTERNAL :: u_fun, m_fun
    EXTERNAL :: ps_lp_and_default
    
    ! In this case, the solution for k_prime can be outside the loop.  
    if (iteration < 1) then
        index_k = 1
    else
        call solve_k_prime(index_k)    
    end if  
    i_k_next = index_k
    k_next_vector = i_k_next

    DO i=1,transfer_num
       total_damage = (AA - A_worst_case) + eshock(i_e_global) * k_epsilon_frac * m_fun(k_initial) 
       transfer_global   = min(total_damage, T_max_grid(i_k_global)) * transfer_grid(i)

       DO j=1,2
            !j==1, no banking crisis| j==2, banking crisis
            index_crisis_global = j            

            CALL ps_lp_and_default(cc,nn,ll,rr)

            transfer_effective = bc_grid(j) * transfer_global
            m_effective =  m_fun(k_initial)* ((1d+0 - bc_grid(j)) + bc_grid(j) * (1d+0 - eshock(i_e_global) * k_epsilon_frac))
            cons_banker = transfer_effective + ll*rr + b_initial + m_effective - k_grid(i_k_next)
            w_vector(i,j) = cons_banker
            v_vector(i,j) = u_fun(cc,nn)
            welfare(i,j)= theta*v_vector(i,j)+(1d+0-theta)*w_vector(i,j)

            cons_vector(i,j) = cc
            labor_vector(i,j) = nn
            rate_vector(i,j) = rr
            tax_vector(i,j) = tax_lp_and_default
    
        END DO
        vector(i) = (1d+0 -pi) *  welfare(i,1) + pi *  welfare(i,2)
    END DO

    index_vector = maxloc(vector) 
    index_opt = index_vector(1) !This is the "Transfer index"

    cons_no_bc = cons_vector(index_opt,1)
    labor_no_bc = labor_vector(index_opt,1)
    rate_no_bc = rate_vector(index_opt,1)
    tax_no_bc = tax_vector(index_opt,1)
    ik_no_bc = k_next_vector(index_opt,1)

    cons_bc = cons_vector(index_opt,2)
    labor_bc = labor_vector(index_opt,2)
    rate_bc = rate_vector(index_opt,2)
    tax_bc = tax_vector(index_opt,2)
    ik_bc = k_next_vector(index_opt,2)

END SUBROUTINE optimize_lp


SUBROUTINE solve_tax_lp_and_default(tax_opt, exit_flag)
    USE param
    IMPLICIT NONE
    INTEGER :: i_tax, counter
    DOUBLE PRECISION :: nn, rev_candidate, tau, tax_base, rev_needed, exit_flag, tax_opt, ERRABS, left,&
    right, dif_left, dif_right, tax_max,  tax_min, d, convergence , transfer_effective, bc
    DOUBLE PRECISION, EXTERNAL :: y_fun, dif_fun_lp_and_default, zbrent

    d=default_grid(i_default_global)
    bc = bc_grid(index_crisis_global)

    ERRABS = 1D-10

    convergence = -1d+0
    exit_flag = 0d+0

    if (indicator_bailout_default > 0.5d+0) then    
        transfer_effective = (1d+0 - bc) *  0d+0 + bc * transfer_global 
    else
        transfer_effective = (1d+0 - d) * ((1d+0 - bc) *  0d+0 + bc * transfer_global) + d * 0d+0 
    endif

    rev_needed = gov_spending+transfer_effective+(1d+0-d)*b_initial*(mrate + (1d+0-mrate)*coupon)
  
    counter=1
    rev_candidate= -1d+0

    IF (rev_needed>0d+0) THEN
       DO WHILE(convergence<0d+0)
            i_tax = counter
            tau = tax_grid(i_tax)
            nn = (EXP(y_initial)*alpha*kapital**(1d+0-alpha)*(1d+0-tau)/(1d+0+gamma_firm*r_min))**(1d+0/(omega-alpha))
            tax_base= nn**(omega)/(1d+0-tau)
            rev_candidate= tau*tax_base

            IF (rev_candidate<rev_needed) THEN
                counter=counter+1
                IF (counter>tax_num) THEN
                    tax_opt=1d+0
                    exit_flag=1d+0
                    convergence=1d+0
                END IF
            ELSE
                convergence=1d+0
            END IF

        END DO

    ELSE
        tax_opt=0d+0
        exit_flag=1d+0
    END IF

    IF (exit_flag<0.5d+0) THEN
        tax_max = tax_grid(counter)
        tax_min = tax_grid(counter-1)

        dif_left  = dif_fun_lp_and_default(tax_min)
        dif_right = dif_fun_lp_and_default(tax_max)

        IF (dif_right*dif_left<0) THEN  !THERE IS AN INTERIOR ROOT
            left =  tax_min
            right = tax_max
            tax_opt = zbrent(dif_fun_lp_and_default,left, right,ERRABS)

        elseif (dif_left<0) THEN
      
            
            WRITE (6,*) 'problems with the zero finder in tax 0'
            tax_opt = tax_min
        ELSE !dif_right>0
            tax_opt = tax_max
        END IF

    END IF

END SUBROUTINE solve_tax_lp_and_default


DOUBLE PRECISION function dif_fun_lp_and_default(tau)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION , INTENT (IN):: tau
    DOUBLE PRECISION :: nn, tax_base, rev_candidate, rev_needed, d, bc, transfer_effective

    d=default_grid(i_default_global)
    bc = bc_grid(index_crisis_global)

    if (indicator_bailout_default > 0.5d+0) then    
        transfer_effective = (1d+0 - bc) *  0d+0 + bc * transfer_global 
    else
        transfer_effective = (1d+0 - d) * ((1d+0 - bc) *  0d+0 + bc * transfer_global) + d * 0d+0 
    endif

    rev_needed = gov_spending+transfer_effective+(1d+0-d)*b_initial*(mrate + (1d+0-mrate)*coupon)

    nn= (EXP(y_initial)*alpha*kapital**(1d+0-alpha)*(1d+0-tau)/(1d+0+gamma_firm*r_min))**(1d+0/(omega-alpha))
    tax_base= nn**(omega)/(1d+0-tau)
    rev_candidate= tau*tax_base

    dif_fun_lp_and_default = rev_needed - rev_candidate

END function dif_fun_lp_and_default


SUBROUTINE solve_tax_full(tax_opt, exit_flag)
    USE param
    IMPLICIT NONE
    INTEGER :: i_tax, counter
    DOUBLE PRECISION :: nn, rev_candidate, tau, tax_base, rev_needed, exit_flag, tax_opt, ERRABS, left, &
    right, dif_left, dif_right, tax_max,  tax_min, d, convergence , transfer_effective, bc, debt_part
    DOUBLE PRECISION, EXTERNAL :: y_fun, dif_fun_full, zbrent

    ERRABS = 1D-10    
    convergence = -1d+0
    exit_flag = 0d+0

    d=default_grid(i_default_global)
    bc = bc_grid(index_crisis_global)        
    
    transfer_effective = (1d+0 - d) * ((1d+0 - bc) *  0d+0 + bc * transfer_global) + d * 0d+0 

    debt_part = b_initial*(mrate + (1d+0-mrate)*coupon) -&
        (b_grid(i_b_next)-(1d+0-mrate)*b_initial) * q_vector(i_b_next, i_k_next, i_y_global)

    rev_needed =  gov_spending + transfer_effective + debt_part

    counter=1
    rev_candidate= -1d+0

    IF (rev_needed>0d+0) THEN
        DO WHILE(convergence<0d+0)
            i_tax=counter
            tau=tax_grid(i_tax)
            nn= (EXP(y_initial)*alpha*kapital**(1d+0-alpha)*(1d+0-tau)/(1d+0+gamma_firm*r_min))**(1d+0/(omega-alpha))
            tax_base= nn**(omega)/(1d+0-tau)
            rev_candidate= tau*tax_base
            IF (rev_candidate<rev_needed) THEN
                counter=counter+1
                IF (counter>tax_num) THEN
                    tax_opt=1d+0
                    exit_flag=1d+0
                    convergence=1d+0
                END IF
            ELSE
                convergence=1d+0
            END IF

        END DO

    ELSE
        tax_opt=0d+0
        exit_flag=1d+0
    END IF

    IF (exit_flag<0.5d+0) THEN
        tax_max = tax_grid(counter)
        tax_min = tax_grid(counter-1)

        dif_left  = dif_fun_full(tax_min)
        dif_right = dif_fun_full(tax_max)

        IF (dif_right*dif_left<0) THEN  !THERE IS AN INTERIOR ROOT
            left =  tax_min
            right = tax_max
            tax_opt = zbrent(dif_fun_full,left, right,ERRABS)
        elseif (dif_left<0) THEN
            ! REMEMBER THIS STAGE SHOULD NEVER OCURR!!!
            ! SO IF I GET THE MESSAGE, SOMETHING HAS TO BE WRONG BEFORE.
            
            WRITE (6,*) 'problems with the zero finder in tax repay'
            tax_opt = tax_min
        ELSE !dif_right>0
            tax_opt = tax_max
        END IF

    END IF

END SUBROUTINE solve_tax_full

DOUBLE PRECISION function dif_fun_full(tau)
    USE param
    IMPLICIT NONE
    DOUBLE PRECISION , INTENT (IN):: tau
    DOUBLE PRECISION :: nn, tax_base, rev_candidate, rev_needed, d, bc, transfer_effective, debt_part

    d = default_grid(i_default_global)
    bc = bc_grid(index_crisis_global)

    transfer_effective = (1d+0 - d) * ((1d+0 - bc) *  0d+0 + bc * transfer_global) + d * 0d+0 

    debt_part = b_initial*(mrate + (1d+0-mrate)*coupon) -&
        (b_grid(i_b_next)-(1d+0-mrate)*b_initial) * q_vector(i_b_next, i_k_next, i_y_global)

    rev_needed =  gov_spending + transfer_effective + debt_part

    nn= (EXP(y_initial)*alpha*kapital**(1d+0-alpha)*(1d+0-tau)/(1d+0+gamma_firm*r_min))**(1d+0/(omega-alpha))
    tax_base= nn**(omega)/(1d+0-tau)
    rev_candidate= tau*tax_base

    dif_fun_full = rev_needed - rev_candidate

END function dif_fun_full

SUBROUTINE solve_k_prime(index_opt_k)
    USE param
    implicit none
    INTEGER, INTENT(OUT) :: index_opt_k
    integer :: index_vector(1),i
    DOUBLE PRECISION :: k_next, vector(k_num),exp_r_next, exp_r_next_excl, aux_1, aux_2, def_today
    DOUBLE PRECISION, EXTERNAL :: m_prime_fun
    
    def_today = default_grid(i_default_global)

    do i=1,k_num
        k_next   = k_grid(i)
        exp_r_next_excl = expected_one_plus_r_tomorrow_from_default(i, i_y_global)
        exp_r_next = expected_one_plus_r_tomorrow_from_repay(i_b_next, i, i_y_global)
        
        aux_1 = def_today * exp_r_next_excl  + (1d+0 - def_today) * exp_r_next
        aux_2 = beta_bank * m_prime_fun(k_next) * aux_1 ! RHS of the optimality condition

        vector(i)= (aux_2 - 1d+0)**(2d+0)

    end do

    index_vector = minloc(vector)    
    index_opt_k = index_vector(1)

END SUBROUTINE solve_k_prime


SUBROUTINE optimize_full(i_opt_t,ib_no_bc, ib_bc,cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, labor_bc, rate_bc, &
tax_bc, output_no_transfer, output_opt_transfer, ik_no_bc, ik_bc)
    USE param
    IMPLICIT NONE
    INTEGER, INTENT (OUT) :: ib_bc, ib_no_bc, i_opt_t, ik_no_bc, ik_bc
    DOUBLE PRECISION, INTENT (OUT) :: cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, labor_bc, rate_bc, tax_bc, &
    output_no_transfer, output_opt_transfer
    INTEGER :: index_vector_debt(1), index_vector_transfer(1),i_t, i_b, j, bp_choice(transfer_num,2),&
    index_k, k_next_vector(b_num, transfer_num, 2)
    DOUBLE PRECISION, DIMENSION(b_num, transfer_num, 2) :: welfare, v_vector, w_vector, cons_vector, labor_vector, &
    rate_vector, tax_vector, output_vector
    DOUBLE PRECISION :: vector_outside(transfer_num),output_outside(transfer_num),cc,nn, ll,rr, cons_banker, transfer_effective, &
    k_next, m_effective, total_damage
    DOUBLE PRECISION, EXTERNAL :: objective_function, obj_banker, y_fun, m_fun
    EXTERNAL :: ps_repay_full

    ! In this case, the solution for k_prime can be outside the loop.

    DO i_b=1,b_num
        i_b_next = i_b
        call solve_k_prime(index_k)     
        i_k_next = index_k
        k_next_vector(i_b, :, :) = index_k
    end do

    DO i_t=1,transfer_num
        total_damage = (AA - A_worst_case) + eshock(i_e_global) * k_epsilon_frac * m_fun(k_initial) 
        transfer_global   = min(total_damage, T_max_grid(i_k_global)) * transfer_grid(i_t)
        ! transfer_global  = min(AA - A_worst_case, T_max) * transfer_grid(i_t)    
        DO j=1,2
            !j==1, no banking crisis | j==2, banking crisis
            index_crisis_global = j            
            DO i_b=1,b_num
                i_b_next = i_b

                ! call solve_k_prime(index_k)     
                ! i_k_next = index_k
                ! k_next_vector(i_b, i_t, j) = index_k

                i_k_next = k_next_vector(i_b, i_t, j)

                CALL ps_repay_full(cc,nn,ll,rr)
                transfer_effective = bc_grid(j) * transfer_global
                m_effective =  m_fun(k_initial)* ((1d+0 - bc_grid(j)) + bc_grid(j) * (1d+0 - eshock(i_e_global)*k_epsilon_frac))
                cons_banker = transfer_effective + ll*rr + b_initial * (mrate + (1d+0-mrate)*coupon) - &
                    q_vector(i_b_next, i_k_next, i_y_global) * (b_grid(i_b_next) - (1d+0-mrate)*b_initial) + &
                    m_effective - k_grid(i_k_next)

                w_vector(i_b, i_t, j) = obj_banker(cons_banker)
                v_vector(i_b, i_t, j) = objective_function(cc,nn)
                welfare(i_b, i_t, j)= theta*v_vector(i_b, i_t, j)+(1d+0-theta)*w_vector(i_b, i_t, j)
                cons_vector(i_b, i_t, j) = cc
                labor_vector(i_b, i_t, j) = nn
                rate_vector(i_b, i_t, j) = rr
                tax_vector(i_b, i_t, j) = tax_repay
                output_vector(i_b, i_t, j) = y_fun(nn)
            END DO
            index_vector_debt = maxloc(welfare(:,i_t,j))   
            bp_choice(i_t,j) = index_vector_debt(1)
        END DO    

        vector_outside(i_t) = (1d+0 -pi) *  welfare(bp_choice(i_t,1),i_t,1) + pi * welfare(bp_choice(i_t,2),i_t, 2)
        output_outside(i_t) = (1d+0 -pi) *  output_vector(bp_choice(i_t,1),i_t,1) + pi * output_vector(bp_choice(i_t,2),i_t,2)
    END DO
         
    

    index_vector_transfer = maxloc(vector_outside) 
    i_opt_t = index_vector_transfer(1) !This is the "Transfer index"

    output_no_transfer = output_outside(1)
    output_opt_transfer = output_outside(i_opt_t)
    ib_no_bc = bp_choice(i_opt_t,1)
    ib_bc = bp_choice(i_opt_t,2)

    cons_no_bc = cons_vector(ib_no_bc, i_opt_t,1)
    labor_no_bc = labor_vector(ib_no_bc, i_opt_t,1)
    rate_no_bc = rate_vector(ib_no_bc, i_opt_t,1)
    tax_no_bc = tax_vector(ib_no_bc, i_opt_t,1)
    ik_no_bc = k_next_vector(ib_no_bc, i_opt_t,1)

    cons_bc = cons_vector(ib_bc, i_opt_t,2)
    labor_bc = labor_vector(ib_bc, i_opt_t,2)
    rate_bc = rate_vector(ib_bc, i_opt_t,2)
    tax_bc = tax_vector(ib_bc, i_opt_t,2)
    ik_bc = k_next_vector(ib_bc, i_opt_t,2)

END SUBROUTINE optimize_full


SUBROUTINE iterate
USE param
IMPLICIT NONE
INTEGER :: i_b, i_y, i, i_e, i_crisis, j, i_opt_t, ib_no_bc, ib_bc, i_b_next_local(2), index_opt_transfer,&
i_k, ik_bc, ik_no_bc, dev_b, i_k_next_local(2)
DOUBLE PRECISION :: convergence, criteria, acum_v, acum_w, acum_v_d,acum_w_d, &
consumption1, labor1,  rr_1, deviation_v, deviation_w, loan1, cons_banker1, dd, bc, &
cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, labor_bc, rate_bc, tax_bc, cons_banker, &
output_no_transfer, output_opt_transfer, m_effective, total_damage(epsilon_num,k_num), guarantee_size
DOUBLE PRECISION, DIMENSION (k_num, y_num, epsilon_num,2) :: vw1_matrix_new, v1_matrix_new,w1_matrix_new
DOUBLE PRECISION, DIMENSION(b_num, k_num, y_num, epsilon_num) :: value_default_matrix_new, value_hh_default_matrix_new,&
value_bank_default_matrix_new , value_repay_matrix_new, value_hh_repay_matrix_new, value_bank_repay_matrix_new, dev_value_matrix,&
value_matrix_new, value_hh_matrix_new, value_bank_matrix_new
DOUBLE PRECISION, DIMENSION(b_num, k_num, y_num, epsilon_num, 2) :: v0_matrix_new, r_matrix_new, w0_matrix_new, vw0_matrix_new, &
v_matrix_new, q_matrix_nodef_new, dev_q_matrix, w_matrix_new, vw_matrix_new
INTEGER, DIMENSION(b_num, k_num, y_num, epsilon_num, 2) :: b_next0_matrix_new, k_next_matrix_new,&
k_next0_matrix, k_next1_matrix
DOUBLE PRECISION, EXTERNAL :: q_fun, objective_function, objective_excl, obj_banker, obj_banker_excl, r_fun, y_fun,&
expected_one_plus_r_fun, m_fun
EXTERNAL :: ps_lp_and_default, optimize_full

criteria = 1d-4   !CRITERIA FOR CONVERGENCE
convergence = -1

DO WHILE(convergence<0)

    iteration = iteration + 1
   
    deviation = 0
    dev_b = 0        
    dev_q = 0        
    deviation_v = 0
    deviation_w = 0

    DO i_y = 1,y_num
        i_y_global = i_y
        y_initial = y_grid(i_y)

        do i_k = 1,k_num
            i_k_global = i_k
            k_initial = k_grid(i_k)

            acum_v_d = 0d+0
            acum_w_d = 0d+0

            DO j=1, epsilon_num ! this is looping over all future epsilon's
                DO i=1,y_num ! this is looping over all future y's
                    acum_v_d = acum_v_d + value_hh_default_matrix(1,i_k,i,j) * trans_matrix(i_y_global, i) * petran(j)
                    acum_w_d = acum_w_d + value_bank_default_matrix(1,i_k,i,j) * trans_matrix(i_y_global,i)* petran(j)
                END DO
            END DO

            exp_v_default(i_k, i_y) = acum_v_d
            exp_w_default(i_k, i_y) = acum_w_d

            i_default_global = 2 !This is the index for the default case
            expected_one_plus_r_tomorrow_from_default(i_k, i_y) = expected_one_plus_r_fun(1, i_k, i_y)
        end do
    end do

    DO i_y = 1,y_num
        i_y_global = i_y
        y_initial = y_grid(i_y)
        
        do i_k = 1,k_num
            i_k_global = i_k
            k_initial = k_grid(i_k)

            DO i_b = 1,b_num
                i_b_global = i_b
                b_initial = b_grid(i_b)

                acum_v = 0d+0
                acum_w = 0d+0
                DO j=1, epsilon_num ! this is looping over all future epsilon's
                    DO i=1,y_num
                        acum_v = acum_v + value_hh_matrix(i_b, i_k, i, j) * trans_matrix(i_y_global, i) * petran(j)
                        acum_w = acum_w + value_bank_matrix(i_b, i_k, i, j) * trans_matrix(i_y_global, i) * petran(j)
                    END DO
                END DO
                exp_v_vector(i_b, i_k, i_y) = acum_v
                exp_w_vector(i_b, i_k, i_y) = acum_w
                q_vector(i_b, i_k, i_y) = q_fun(i_b, i_k, i_y)

                i_default_global = 1 !This is the index for the no-default case
                expected_one_plus_r_tomorrow_from_repay(i_b, i_k, i_y) = expected_one_plus_r_fun(i_b, i_k, i_y)
            end do

        END DO
    END DO
        
    DO i_e = 1, epsilon_num
        i_e_global = i_e
        DO i_k = 1,k_num
            i_k_global = i_k
            k_initial = k_grid(i_k)
        
            total_damage(i_e, i_k) = eshock(i_e_global) *(AA + k_epsilon_frac * m_fun(k_initial))
        end do
    end do

    i_default_global = 2 !Country defaults

    DO i_e = 1, epsilon_num
        i_e_global = i_e
        A_worst_case = AA * (1d+0 - eshock(i_e_global))

        DO i_y = 1,y_num
            i_y_global = i_y
            y_initial = y_grid(i_y)

            DO i_k = 1,k_num
                i_k_global = i_k
                k_initial = k_grid(i_k)

                IF (indicator_bailout_default > 0.5d+0) then
                    i_b_global = 1
                    b_initial = b_grid(1)
                    i_b_next = 1 !There is no borrowing in default
                    i_default_global = 2  ! country defaults

                    CALL optimize_lp(index_opt_transfer, cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, &
                    labor_bc, rate_bc, tax_bc, ik_no_bc, ik_bc)
                    i_k_next_local(1) = ik_no_bc
                    i_k_next_local(2) = ik_bc                                        
                                        
                    DO i_crisis=1,2               
                        bc = bc_grid(i_crisis)
                        m_effective =  m_fun(k_initial) * ((1d+0 - bc) + bc * (1d+0 - eshock(i_e_global) * k_epsilon_frac))
                        consumption1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * cons_no_bc + bc * cons_bc 
                        labor1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * labor_no_bc + bc * labor_bc 
                        r1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * rate_no_bc + bc * rate_bc 
                        tax1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * tax_no_bc + bc * tax_bc                                                 
                        guarantee1_matrix(i_k,i_y,i_e,i_crisis) = &
                            min(total_damage(i_e,i_k), T_max_grid(i_k_global)) * transfer_grid(index_opt_transfer)  
                        
                        transfer1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * 0d+0 +   &
                            bc * guarantee1_matrix(i_k,i_y,i_e,i_crisis)
                        
                        loan1_matrix(i_k,i_y,i_e,i_crisis) = gamma_firm * alpha * y_fun(labor1_matrix(i_k,i_y,i_e,i_crisis))/&
                            (1d+0 + r1_matrix(i_k,i_y,i_e,i_crisis)*gamma_firm)
                
                        v1_matrix_new(i_k,i_y,i_e,i_crisis) = objective_excl(consumption1_matrix(i_k,i_y,i_e,i_crisis), &
                            labor1_matrix(i_k,i_y,i_e,i_crisis))
                        k_next1_matrix(:,i_k,i_y,i_e,i_crisis) = i_k_next_local(i_crisis)
                        
                        cons_banker = transfer1_matrix(i_k,i_y,i_e,i_crisis) + & 
                            loan1_matrix(i_k,i_y,i_e,i_crisis) * r1_matrix(i_k,i_y,i_e,i_crisis) + &
                            m_effective - k_grid(i_k_next_local(i_crisis))

                        cons_bank1_matrix(i_k,i_y,i_e,i_crisis) = cons_banker
                        w1_matrix_new(i_k,i_y,i_e,i_crisis)  = obj_banker_excl(cons_banker)
                        vw1_matrix_new(i_k,i_y,i_e,i_crisis)= theta*v1_matrix_new(i_k,i_y,i_e,i_crisis) + &
                            (1.0d+0 - theta)* w1_matrix_new(i_k,i_y,i_e,i_crisis)
                    END DO ! i_crisis
                
                ELSE ! no bailout during default
             
                    DO i_crisis=1,2
                        ! index_crisis_global:1 == no crisis; index_crisis_global:2 == crisis.
                        bc = bc_grid(i_crisis)
                        m_effective =  m_fun(k_initial) * ((1d+0 - bc) + bc * (1d+0 - eshock(i_e_global) * k_epsilon_frac))
                        index_crisis_global = i_crisis
                        b_initial = b_grid(1) ! I DO this just to have b_initial with a value in the default section of the code
                        i_b_next = 1 !I am telling the code that this guy is starting with zero debt next period (in case it is redeemed).
                        transfer_global = 0.0d+0 ! There is no bailout under default.

                        call solve_k_prime(i_k_next)
                        k_next1_matrix(:, i_k,i_y,i_e,i_crisis) = i_k_next

                        CALL ps_lp_and_default(consumption1,labor1,loan1, rr_1)

                        v1_matrix_new(i_k,i_y,i_e,i_crisis) = objective_excl(consumption1,labor1)
                        cons_banker1 = loan1 * rr_1 + m_effective - k_grid(i_k_next)

                        cons_bank1_matrix(i_k,i_y,i_e,i_crisis) = cons_banker1
                        w1_matrix_new(i_k,i_y,i_e,i_crisis) = obj_banker_excl(cons_banker1)
                        vw1_matrix_new(i_k,i_y,i_e,i_crisis)= theta*v1_matrix_new(i_k,i_y,i_e,i_crisis) +&
                            (1.0d+0 - theta)* w1_matrix_new(i_k,i_y,i_e,i_crisis)
                        r1_matrix(i_k,i_y,i_e,i_crisis) = rr_1
                        labor1_matrix(i_k,i_y,i_e,i_crisis) = labor1
                        consumption1_matrix(i_k,i_y,i_e,i_crisis) = consumption1
                        loan1_matrix(i_k,i_y,i_e,i_crisis) = loan1
                        tax1_matrix(i_k,i_y,i_e,i_crisis) = tax_lp_and_default
                    END DO
         
                END IF

                value_hh_default_matrix_new(:, i_k, i_y, i_e) = (1d+0-pi)*v1_matrix_new(i_k,i_y,i_e,1) + &
                pi *v1_matrix_new( i_k,i_y,i_e,2)
                value_bank_default_matrix_new(:, i_k, i_y, i_e) = (1d+0-pi)*w1_matrix_new(i_k,i_y,i_e,1) + &
                pi *w1_matrix_new( i_k,i_y,i_e,2)
                value_default_matrix_new(:, i_k, i_y, i_e) = (1d+0-pi)*vw1_matrix_new(i_k,i_y,i_e,1) + &
                pi *vw1_matrix_new( i_k,i_y,i_e,2)
            end do
        END DO
    END DO

    i_default_global=1   ! Country does not default and is not excluded for the next period

    DO i_e = 1, epsilon_num
        i_e_global = i_e
        A_worst_case = AA * (1d+0 - eshock(i_e_global))
      
        DO i_y = 1,y_num
            i_y_global = i_y
            y_initial = y_grid(i_y)
            
            DO i_k = 1,k_num
                i_k_global = i_k
                k_initial = k_grid(i_k)                

                DO i_b = 1,b_num    
                    i_b_global = i_b
                    b_initial = b_grid(i_b)

                    CALL optimize_full(i_opt_t, ib_no_bc, ib_bc, cons_no_bc, &
                    labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, labor_bc,   &
                    rate_bc, tax_bc, output_no_transfer, output_opt_transfer, ik_no_bc, ik_bc)
            
                    output_mat_no_transfer (i_b,i_k,i_y,i_e) = output_no_transfer
                    output_mat_opt_transfer(i_b,i_k,i_y,i_e) = output_opt_transfer
   
                    i_b_next_local(1) = ib_no_bc
                    i_b_next_local(2) = ib_bc

                    i_k_next_local(1) = ik_no_bc
                    i_k_next_local(2) = ik_bc

                    guarantee_size = min(total_damage(i_e, i_k), T_max_grid(i_k_global)) * transfer_grid(i_opt_t)
                    guarantee0_matrix(i_b, i_k, i_y,i_e,:) = guarantee_size

                    DO i_crisis=1,2
                        index_crisis_global = i_crisis !I think it's probably redundant here.
                        bc = bc_grid(i_crisis)
                        
                        m_effective =  m_fun(k_initial) * ((1d+0 - bc) + bc * (1d+0 - eshock(i_e_global) * k_epsilon_frac))

                        b_next0_matrix_new(i_b,i_k, i_y,i_e,i_crisis) = i_b_next_local(i_crisis)
                        k_next0_matrix(i_b,i_k, i_y,i_e,i_crisis) =i_k_next_local(i_crisis)
            
                        consumption0_matrix(i_b,i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * cons_no_bc + bc * cons_bc 
                        labor0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * labor_no_bc + bc * labor_bc 
                        r0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * rate_no_bc + bc * rate_bc 
                        tax0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * tax_no_bc + bc * tax_bc                                                                     
                           
                        transfer0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * 0d+0 + bc * guarantee_size

                        loan0_matrix(i_b, i_k, i_y,i_e,i_crisis) = gamma_firm * alpha * &
                        y_fun(labor0_matrix(i_b, i_k, i_y,i_e,i_crisis))/&
                        (1d+0 + r0_matrix(i_b, i_k, i_y,i_e,i_crisis)*gamma_firm)

                        i_b_next = i_b_next_local(i_crisis) !I need this global variable invoked correctly               
                        q_matrix_nodef_new(i_b, i_k, i_y, i_e, i_crisis) = q_vector(i_b_next, i_k_next_local(i_crisis), i_y_global)

                        v0_matrix_new(i_b, i_k, i_y,i_e,i_crisis)  = &
                            objective_function(consumption0_matrix(i_b, i_k, i_y,i_e,i_crisis),&
                            labor0_matrix(i_b, i_k, i_y,i_e,i_crisis))
                        ! This was a leftover mistake, without much repercussion:
                        cons_banker = transfer0_matrix(i_b, i_k, i_y,i_e,i_crisis) + b_initial * (mrate + (1d+0-mrate)*coupon) + &
                            loan0_matrix(i_b, i_k, i_y,i_e,i_crisis) * r0_matrix(i_b, i_k, i_y,i_e,i_crisis) - &
                            q_matrix_nodef_new(i_b, i_k, i_y, i_e, i_crisis) * (b_grid(i_b_next) - (1d+0-mrate)*b_initial) +&
                            m_effective - k_grid(k_next0_matrix(i_b, i_k, i_y,i_e,i_crisis))

                        cons_bank0_matrix(i_b, i_k, i_y,i_e,i_crisis) = cons_banker               
                        w0_matrix_new(i_b,i_k,i_y,i_e,i_crisis)  = obj_banker(cons_banker)         
               
                        vw0_matrix_new(i_b, i_k, i_y,i_e,i_crisis) = theta*v0_matrix_new(i_b,i_k,i_y,i_e,i_crisis) + &
                            (1.0d+0 - theta)* w0_matrix_new(i_b,i_k,i_y,i_e,i_crisis)

                        dev_q = MAX(dev_q, ABS(q_matrix_nodef_new(i_b, i_k, i_y, i_e, i_crisis) - &
                            q_matrix_nodef(i_b, i_k, i_y, i_e, i_crisis)))

                        dev_q_matrix(i_b, i_k, i_y, i_e, i_crisis) = &
                            ABS(q_matrix_nodef_new(i_b,i_k, i_y, i_e, i_crisis) - q_matrix_nodef(i_b, i_k, i_y, i_e, i_crisis)) 

                        dev_b = MAX(dev_b, IABS(b_next0_matrix_new(i_b, i_k, i_y, i_e, i_crisis) - &
                            b_next0_matrix(i_b, i_k, i_y, i_e, i_crisis))) 

                    END DO                     

                    value_hh_repay_matrix_new(i_b, i_k ,i_y, i_e) = (1d+0-pi)*v0_matrix_new(i_b, i_k,i_y, i_e,1) + &
                        pi *v0_matrix_new(i_b, i_k,i_y, i_e,2)
                    value_bank_repay_matrix_new(i_b, i_k,i_y, i_e) = (1d+0-pi)*w0_matrix_new(i_b, i_k,i_y, i_e,1) + &
                        pi *w0_matrix_new(i_b, i_k,i_y, i_e,2)
                    value_repay_matrix_new(i_b, i_k,i_y, i_e) = (1d+0-pi)*vw0_matrix_new(i_b, i_k,i_y, i_e,1) + &
                        pi *vw0_matrix_new(i_b, i_k,i_y, i_e,2)

                    IF (value_default_matrix_new(i_b, i_k, i_y, i_e) > value_repay_matrix_new(i_b, i_k, i_y, i_e)) THEN
                        default_decision_new(i_b, i_k, i_y, i_e) = 2
                        value_matrix_new(i_b,i_k,i_y,i_e) = value_default_matrix_new(i_b,i_k,i_y,i_e)
                        value_hh_matrix_new(i_b,i_k,i_y,i_e) = value_hh_default_matrix_new(i_b,i_k,i_y,i_e)
                        value_bank_matrix_new(i_b,i_k,i_y,i_e) = value_bank_default_matrix_new(i_b,i_k,i_y,i_e)
                        r_matrix_new(i_b, i_k, i_y, i_e, :) = r1_matrix(i_k, i_y,i_e,:)
                        k_next_matrix_new(i_b, i_k, i_y, i_e, :) = k_next1_matrix(i_b,i_k, i_y,i_e,:)
                        vw_matrix_new(i_b,i_k,  i_y, i_e, :) = vw1_matrix_new(i_k, i_y,i_e,:) 
                        v_matrix_new(i_b, i_k, i_y, i_e, :) = v1_matrix_new(i_k,i_y,i_e,:) 
                        w_matrix_new(i_b, i_k, i_y, i_e, :) = w1_matrix_new(i_k,i_y,i_e,:) 

                    ELSE
                        default_decision_new(i_b,i_k,i_y,i_e) = 1
                        value_matrix_new(i_b,i_k,i_y,i_e) = value_repay_matrix_new(i_b,i_k,i_y,i_e)
                        value_hh_matrix_new(i_b,i_k,i_y,i_e) = value_hh_repay_matrix_new(i_b,i_k,i_y,i_e)
                        value_bank_matrix_new(i_b,i_k,i_y,i_e) = value_bank_repay_matrix_new(i_b,i_k,i_y,i_e)
                        r_matrix_new (i_b, i_k, i_y, i_e, :) = r0_matrix(i_b,i_k,i_y,i_e,:)
                        k_next_matrix_new(i_b, i_k, i_y, i_e, :) = k_next0_matrix(i_b,i_k, i_y,i_e,:)
                        vw_matrix_new(i_b, i_k, i_y, i_e, :) = vw0_matrix_new(i_b,i_k,i_y,i_e,:)
                        v_matrix_new (i_b, i_k, i_y, i_e, :) = v0_matrix_new(i_b,i_k,i_y,i_e,:)
                        w_matrix_new (i_b, i_k, i_y, i_e, :) = w0_matrix_new(i_b,i_k,i_y,i_e,:)

                    END IF
            
                    deviation = MAX(deviation, ABS(value_matrix_new(i_b,i_k,i_y,i_e) - value_matrix(i_b,i_k,i_y,i_e)))
                    dev_value_matrix(i_b,i_k, i_y, i_e) = ABS(value_matrix_new(i_b,i_k,i_y,i_e) - value_matrix(i_b,i_k,i_y,i_e))
                END DO ! i_b
            END DO ! i_k    
        END DO !i_y
    END DO ! i_e

   

    WRITE(nout, *) iteration, deviation, dev_q, dev_b
   
    
    !4) UPDATE VALUES OF MATRICES
    default_decision = default_decision_new 
    value_matrix = (1d+0 - damp_v) * value_matrix_new + damp_v * value_matrix
    q_matrix_nodef = (1d+0 - damp_q) * q_matrix_nodef_new + damp_q * q_matrix_nodef
    value_hh_matrix = value_hh_matrix_new
    value_bank_matrix = value_bank_matrix_new
    value_repay_matrix = value_repay_matrix_new
    value_hh_repay_matrix = value_hh_repay_matrix_new
    value_bank_repay_matrix = value_bank_repay_matrix_new
    value_default_matrix = value_default_matrix_new
    value_hh_default_matrix = value_hh_default_matrix_new
    value_bank_default_matrix = value_bank_default_matrix_new
    vw0_matrix  = vw0_matrix_new
    vw1_matrix  = vw1_matrix_new
    vw_matrix   = vw_matrix_new
    v0_matrix  = v0_matrix_new
    v1_matrix  = v1_matrix_new
    v_matrix   = v_matrix_new
    w0_matrix  = w0_matrix_new
    w1_matrix  = w1_matrix_new
    w_matrix   = w_matrix_new    
    r_matrix = r_matrix_new
    b_next0_matrix = b_next0_matrix_new
    k_next_matrix = k_next_matrix_new

    IF (writeout.EQ.1) THEN
        OPEN (2, FILE='graphs_q_matrix_nodef.txt')
        WRITE(2, *) q_matrix_nodef
        CLOSE (2)
    ENDIF
   
    IF (deviation < criteria .OR. iteration >= maxiter) THEN
        IF (writeout.EQ.1) THEN
            OPEN (7, FILE='graphs_value_planner_dss.txt')
            OPEN (8, FILE='graphs_value_hh_dss.txt')
            OPEN (9, FILE='graphs_value_bank_dss.txt')
            OPEN (10, FILE='graphs_v_dss.txt')
            OPEN (11, FILE='graphs_default_dss.txt')
            OPEN (26, FILE='graphs_vw_dss.txt')
            OPEN (27, FILE='graphs_w_dss.txt')
            OPEN (28, FILE='graphs_interest.txt')
            OPEN (29, FILE='graphs_consumption.txt')
            OPEN (30, FILE='graphs_cons_bank.txt')
            OPEN (32, FILE='graphs_labor.txt')
            OPEN (40, FILE='graphs_taxes.txt')
            OPEN (41, FILE='graphs_loans.txt')
            OPEN (42, FILE='graphs_transfers.txt')
            OPEN (43, FILE='graphs_q.txt')
            OPEN (44, FILE='graphs_deviation.txt')
            OPEN (45, FILE='graphs_b_next.txt')
            OPEN (46, FILE='graphs_output.txt')
            OPEN (47, FILE='graphs_guarantees.txt')
            OPEN (48, FILE='graphs_k_next.txt')
        ENDIF
      
        consumption_matrix = 0.0d+0
        cons_bank_matrix = 0.0d+0
        b_next_matrix = 1
        
        DO i_crisis = 1,2
            index_crisis_global = i_crisis !Also redundant here

            DO i_e = 1,epsilon_num
                i_e_global = i_e
         
                DO i_y = 1,y_num
                    i_y_global=i_y
                    y_initial = y_grid(i_y)

                    DO i_k =1, k_num
                        i_k_global = i_k
                        k_initial = k_grid(i_k)                
         
                        DO i_b = 1,b_num
                            i_b_global = i_b
                            b_initial = b_grid(i_b)
                        
                            dd = default_grid(default_decision(i_b,i_k,i_y,i_e))

                            n_matrix(i_b, i_k, i_y, i_e, i_crisis) = dd * labor1_matrix(i_k,i_y,i_e,i_crisis) +&
                            (1d+0 - dd) * labor0_matrix(i_b, i_k, i_y,i_e,i_crisis)
                
                            loan_matrix(i_b, i_k, i_y, i_e, i_crisis) = dd * loan1_matrix(i_k,i_y,i_e,i_crisis) + &                
                            (1d+0 - dd) * loan0_matrix(i_b, i_k, i_y,i_e,i_crisis)
                
                            IF (dd > 0.5d+0) THEN
                                IF (dd*indicator_bailout_default > 0.5d+0) THEN
                                    transfer_matrix(i_b, i_k, i_y, i_e, i_crisis) = transfer1_matrix(i_k,i_y,i_e,i_crisis)
                                    guarantee_matrix(i_b, i_k, i_y, i_e, i_crisis) = guarantee1_matrix(i_y,i_k, i_e,i_crisis)
                                ELSE
                                    transfer_matrix(i_b, i_k, i_y, i_e, i_crisis) = 0d+0 !A government  excluded cannot make transfers
                                    guarantee_matrix(i_b, i_k, i_y, i_e, i_crisis) = 0d+0 !A government     excluded cannot make transfers
                                END IF

                            ELSE
                                transfer_matrix(i_b, i_k, i_y, i_e, i_crisis) = transfer0_matrix(i_b, i_k, i_y,i_e,i_crisis)
                                guarantee_matrix(i_b, i_k, i_y, i_e, i_crisis) = guarantee0_matrix(i_b, i_k, i_y,i_e,i_crisis)
                            END IF
                
                            consumption_matrix(i_b, i_k, i_y, i_e, i_crisis) = dd * consumption1_matrix(i_k,i_y,i_e,i_crisis) &
                            + (1d+0 - dd) * consumption0_matrix(i_b, i_k, i_y,i_e,i_crisis)
                
                            cons_bank_matrix(i_b, i_k, i_y, i_e, i_crisis) = dd * cons_bank1_matrix(i_k,i_y,i_e,i_crisis) &
                            + (1d+0 - dd) * cons_bank0_matrix(i_b, i_k, i_y,i_e,i_crisis)

                            IF (dd<0.5d+0) b_next_matrix(i_b, i_k,i_y,i_e,i_crisis) = b_next0_matrix(i_b, i_k,i_y,i_e,i_crisis)
                
                            IF (writeout.EQ.1) THEN
                                WRITE(7, *)  value_matrix_new(i_b,i_k,i_y,i_e), value_repay_matrix_new(i_b,i_k,i_y,i_e), &
                                value_default_matrix_new(i_b,i_k,i_y,i_e)
                    
                                WRITE(8, *)  value_hh_matrix_new(i_b,i_k,i_y,i_e), value_hh_repay_matrix_new(i_b,i_k,i_y,i_e),  &
                                value_hh_default_matrix_new(i_b,i_k,i_y,i_e)
                    
                                WRITE(9, *)  value_bank_matrix_new(i_b,i_k,i_y,i_e), value_bank_repay_matrix_new(i_b,i_k,i_y,i_e),&
                                value_bank_default_matrix_new(i_b,i_k,i_y,i_e)
                                WRITE(11, *) dd
                                WRITE(26, *) vw_matrix_new(i_b,i_k,i_y,i_e,i_crisis), vw0_matrix_new(i_b,i_k,i_y,i_e,i_crisis), &
                                vw1_matrix_new(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(10, *)  v_matrix_new(i_b,i_k,i_y,i_e,i_crisis),  v0_matrix_new(i_b,i_k,i_y,i_e,i_crisis), &
                                v1_matrix_new(i_k,i_y,i_e,i_crisis)
                                WRITE(27, *)  w_matrix_new(i_b,i_k,i_y,i_e,i_crisis),  w0_matrix_new(i_b,i_k,i_y,i_e,i_crisis), &
                                w1_matrix_new(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(28,*) r_matrix_new(i_b,i_k,i_y,i_e,i_crisis), r0_matrix(i_b, i_k, i_y,i_e,i_crisis), &
                                    r1_matrix(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(29,*) consumption_matrix(i_b, i_k, i_y, i_e, i_crisis),&
                                consumption0_matrix(i_b, i_k, i_y,i_e,i_crisis), consumption1_matrix(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(30,*) cons_bank_matrix(i_b, i_k, i_y, i_e, i_crisis), &
                                cons_bank0_matrix(i_b, i_k, i_y,i_e,i_crisis), cons_bank1_matrix(i_k,i_y,i_e,i_crisis)
                        
                                WRITE(32,*) n_matrix(i_b,i_k,i_y,i_e,i_crisis), labor0_matrix(i_b, i_k, i_y,i_e,i_crisis),&
                                labor1_matrix(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(40,*) dd * tax1_matrix(i_k,i_y,i_e,i_crisis) + &
                                (1d+0 - dd) * tax0_matrix(i_b, i_k, i_y,i_e,i_crisis),&
                                tax0_matrix(i_b, i_k, i_y,i_e,i_crisis), tax1_matrix(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(41,*) loan_matrix(i_b,i_k,i_y,i_e,i_crisis), loan0_matrix(i_b,i_k,i_y,i_e,i_crisis),&
                                loan1_matrix(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(42,*) transfer_matrix(i_b,i_k,i_y,i_e,i_crisis), transfer0_matrix(i_b,i_k,i_y,i_e,i_crisis),&
                                transfer1_matrix(i_k,i_y,i_e,i_crisis)
                    
                                WRITE(43, *) q_vector(i_b, i_k, i_y), q_matrix_nodef_new(i_b,i_k,i_y,i_e,i_crisis)
                    
                                WRITE(44, *) dev_value_matrix(i_b, i_k, i_y, i_e),dev_q_matrix(i_b, i_k, i_y, i_e, i_crisis)  
                    
                                WRITE(45, *) b_next_matrix(i_b, i_k, i_y,i_e,i_crisis), b_next0_matrix(i_b, i_k, i_y,i_e,i_crisis)

                                IF (i_crisis==2) THEN
                                    WRITE(46,*) transfer_matrix(i_b, i_k, i_y,i_e,i_crisis),&
                                    output_mat_no_transfer(i_b,i_k,i_y,i_e), &
                                    output_mat_opt_transfer(i_b,i_k,i_y,i_e)
                                END IF
                                WRITE(47,*) guarantee_matrix(i_b,i_k,i_y,i_e,i_crisis),&
                                guarantee0_matrix(i_b,i_k,i_y,i_e,i_crisis),&
                                guarantee1_matrix(i_k,i_y,i_e,i_crisis)

                                WRITE(48, *) k_next_matrix(i_b, i_k, i_y,i_e,i_crisis), k_next0_matrix(i_b, i_k, i_y,i_e,i_crisis),&
                                k_next1_matrix(i_b, i_k, i_y, i_e, i_crisis)
                            ENDIF
                
                        END DO ! i_b
                    END DO ! i_k
                END DO ! i_y
            END DO ! i_e
        END DO ! i_crisis

        IF (writeout.EQ.1) THEN
            CLOSE (7)
            CLOSE (8)
            CLOSE (9)
            CLOSE (10)
            CLOSE (11)
            CLOSE (26)
            CLOSE (27)
            CLOSE (28)
            CLOSE (29)
            CLOSE (30)
            CLOSE (32)
            CLOSE (40)
            CLOSE (41)
            CLOSE (42)
            CLOSE (43)
            CLOSE (44)
            CLOSE (45)
            CLOSE (46)
            CLOSE (47)
            CLOSE (48)
        ENDIF
      convergence =1 ! Declare convergence =)

    END IF !CLOSES THE LOOP THAT CHECKS CONVERGENCE

END DO !CLOSES THE WHILE LOOP

END SUBROUTINE iterate

program main
USE param
IMPLICIT NONE
INTEGER  i_b , i_y , index_opt_transfer, i_e, i_crisis,i_k, ik_bc, ik_no_bc, i_k_next_local(2)
DOUBLE PRECISION :: start_time, end_time, consumption1,  labor1, r1, loan1, &
    cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc, cons_bc, labor_bc, rate_bc, tax_bc, bc, dum, &
    dummy_1, dummy_2, m_effective, total_damage(epsilon_num, k_num)
DOUBLE PRECISION, EXTERNAL :: u_fun, y_fun, m_fun
EXTERNAL ::  compute_grid, ps_lp_and_default, optimize_lp

!! ARGUMENTS FROM COMMAND LINE
CHARACTER (LEN = 80)    :: cmdline      ! generic command line  

!! ARGUMENTS FOR PROGRAM
INTEGER                 :: num          ! case selected on menu
INTEGER                 :: argtot, N_tot
INTEGER                 :: ios
CHARACTER(LEN=12) string, FORMAT
CHARACTER(LEN=19)       :: path
LOGICAL                 :: file_exists
INTEGER, DIMENSION(5)   :: idx_vec, vec_size

!! MAIN PROGRAM    
path = '.'

IF (indicator_bailout_default>0.5d+0) THEN
    beta = 0.89911338d+0
    AA = 0.21178885d+0
    gov_spending = 0.14646447d+0
    sig_e = 4.9444444d+0
ENDIF

! Get command COUNT
argtot      = COMMAND_ARGUMENT_COUNT()

ifcmdargs: IF (argtot > 0) THEN
        
    writeout = 0
    
    CALL GET_COMMAND_ARGUMENT(NUMBER = 1, VALUE = cmdline)
    READ (cmdline,*) num
    WRITE (*,*) ' Program run mode is: ', num	
    
    N_tot = N_bita*N_abar*N_sigmae*N_gov*N_tmax
    PRINT *, N_tot    
    vec_size = (/N_bita,N_abar,N_sigmae,N_gov,N_tmax/)
    idx_vec = ind2sub(num,vec_size)
    
    IF (N_bita>1) THEN
        CALL creategrid(bita_grid, N_bita, 0.78d+0, 0.83d+0, 0.88d+0, 1.5d+0, 1.0d+0)
        !PRINT *, 'bita_grid:', bita_grid 
        beta = bita_grid(idx_vec(1))
    END IF
    
    IF (N_abar>1) THEN
        CALL creategrid(abar_grid, N_abar, 0.23d+0, 0.28d+0, 0.33d+0, 1.5d+0, 1.0d+0)
        !PRINT *, 'abar_grid:', abar_grid
        AA = abar_grid(idx_vec(2))
    END IF
    
    IF (N_sigmae>1) THEN    
        CALL creategrid(sig_e_grid, N_sigmae, 3.0d+0, 3.0d+0, 15.0d+0, 1.5d+0, 1.0d+0)
        !PRINT *, 'sig_e_grid:', sig_e_grid
        sig_e = sig_e_grid(idx_vec(3))
    END IF
    
    IF (N_gov>1) THEN  
        CALL creategrid(gov_grid, N_gov, 0.14d+0, 0.15d+0, 0.16d+0, 1.5d+0, 1.0d+0)
        !PRINT *, 'gov_grid:', gov_grid
        gov_spending = gov_grid(idx_vec(4)) 
    END IF
    
    IF (N_tmax>1) THEN 
        CALL creategrid(tmax_grid, N_tmax, 0.00d+0, 0.00d+0, 1.00d+0, 1.0d+0, 1.0d+0)
        !PRINT *, 'tmax_grid:', tmax_grid
        T_max_frac = tmax_grid(idx_vec(5))  
    END IF  
    
    DATA FORMAT /'(I6.6)'/
    WRITE(string,FORMAT) num
    path = 'output/config'//TRIM(string)	
    CALL EXECUTE_COMMAND_LINE('mkdir -p '//path) !ios = SYSTEM( 'mkdir -p '//path )
    PRINT *, 'path:',path  
    
ELSE
    
    num = 1   
        
END IF ifcmdargs  

open (1, FILE='param_calib_scale_mk.txt')
READ(1, *) scale_mk
close (1)
scale_mk = scale_mk/100d+0

open (1, FILE='param_calib_AA.txt')
READ(1, *) AA
close (1)
AA = AA/1000d+0

open (1, FILE='param_calib_alpha_k.txt')
READ(1, *) alpha_k
close (1)
alpha_k = alpha_k/100d+0

open (2, FILE='param_calib_k_epsilon_frac.txt')
READ(2, *) k_epsilon_frac
close (2)
k_epsilon_frac = k_epsilon_frac/100d+0

open (2, FILE='param_calib_beta.txt')
READ(2, *) beta
close (2)
beta = beta/100d+0

PRINT *, 'beta:', beta
PRINT *, 'abar:', AA
PRINT *, 'sig_e:',sig_e 
PRINT *, 'gov_spending:', gov_spending 
PRINT *, 'T_max_frac:', T_max_frac
PRINT *, 'mrate:', mrate
PRINT *, 'k_epsilon_frac:', k_epsilon_frac
PRINT *, 'alpha_k:', alpha_k
PRINT *, 'scale_mk:', scale_mk


IF (indicator_external>0.5d+0) THEN
    PRINT *, 'Read in external guesses: YES' 
ELSE
    PRINT *, 'Read in external guesses: NO' 
ENDIF

CALL cpu_time(start_time)
CALL compute_grid

IF (writeout.EQ.1) THEN
    OPEN (2, FILE='graphs_parameters.txt')
    WRITE(2,'(13F12.8)') alpha, beta, beta_bank, sigma, prob_excl_end, theta, gamma_firm, omega, gov_spending, AA, pi, sig_e,&
     T_max_frac
    CLOSE (2)
ENDIF


indicator_lumpsum = 0d+0

IF (simulonly.EQ.1) indicator_external = 1d+0

IF (indicator_external < 0.5d+0) THEN

    ! 1) INITIALIZE PRICE MATRICES
    iteration = 0
    b_next0_matrix = 1 ! Telling the code that there is no borrowing in the one-period economy (== last period in the finite horizon)
    ! That gets overwritten later.
    k_next_matrix = 1 ! Telling the code that there is saving into the storage tech in the last period.
    i_k_next = 1 ! Telling the code that there is no saving into the storage tech in the one-period economy (== last period in the finite horizon)
    q_matrix_nodef = 0d+0
    q_vector = 0d+0
    
    DO i_e = 1, epsilon_num
        i_e_global = i_e        
        DO i_k = 1,k_num
            i_k_global = i_k
            k_initial = k_grid(i_k)
        
            total_damage(i_e, i_k) = eshock(i_e_global) *(AA + k_epsilon_frac * m_fun(k_initial))            
        end do
    end do

    default_decision = 2

    DO i_e = 1,epsilon_num
        i_e_global = i_e
        A_worst_case = AA * (1d+0 - eshock(i_e_global))

        DO i_y = 1,y_num
            i_y_global=i_y
            y_initial = y_grid(i_y)

            do i_k = 1, k_num
                k_initial = k_grid(i_k)
                i_k_global  = i_k
         
                IF (indicator_bailout_default > 0.5d+0) THEN

                    i_b_global = 1
                    b_initial = b_grid(1)
                    i_b_next = 1 !There is no borrowing in the one-period economy (== last period in the    finite horizon)
                    i_default_global = 2  ! country defaults
                
                    CALL optimize_lp(index_opt_transfer, cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc,cons_bc, labor_bc, &
                    rate_bc, tax_bc, ik_no_bc, ik_bc)

                    i_k_next_local(1) = ik_no_bc
                    i_k_next_local(2) = ik_bc

                    DO i_crisis=1,2
                        !index_crisis_global:1 == no crisis; index_crisis_global:2 == crisis.
                    
                        bc = bc_grid(i_crisis)
                        
                        m_effective =  m_fun(k_initial) * ((1d+0 - bc) + bc * (1d+0 - eshock(i_e_global) * k_epsilon_frac))
                        consumption1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * cons_no_bc + bc * cons_bc 
                        labor1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * labor_no_bc + bc * labor_bc 
                        r1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * rate_no_bc + bc * rate_bc 
                        tax1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * tax_no_bc + bc * tax_bc                                               

                        guarantee1_matrix(i_k,i_y,i_e,i_crisis) = &
                        min(total_damage(i_e_global,i_k_global),T_max_grid(i_k))*transfer_grid(index_opt_transfer)  

                        transfer1_matrix(i_k,i_y,i_e,i_crisis) = (1d+0 - bc) * 0d+0 +   &
                        bc * guarantee1_matrix(i_k,i_y,i_e,i_crisis)
                        
                        loan1_matrix(i_k,i_y,i_e,i_crisis) = gamma_firm * alpha * y_fun(labor1_matrix(i_k,i_y,i_e,i_crisis))/&
                        (1d+0 + r1_matrix(i_k,i_y,i_e,i_crisis)*gamma_firm)
            
                        v1_matrix(i_k,i_y,i_e,i_crisis)  = u_fun(consumption1_matrix(i_k,i_y,i_e,i_crisis), &
                        labor1_matrix(i_k,i_y,i_e,i_crisis))
                    
                        w1_matrix(i_k,i_y,i_e,i_crisis)  = transfer1_matrix(i_k,i_y,i_e,i_crisis) +&
                        loan1_matrix(i_k,i_y,i_e,i_crisis) * r1_matrix(i_k,i_y,i_e,i_crisis) + &
                        m_effective - k_grid(i_k_next_local(i_crisis))

                        vw1_matrix(i_k,i_y,i_e,i_crisis) = theta*v1_matrix(i_k,i_y,i_e,i_crisis) + &
                        (1.0d+0 - theta)*w1_matrix(i_k,i_y,i_e,i_crisis)
            
                    END DO ! i_crisis
            
                ELSE ! no bailout during default
                    DO i_crisis=1,2
                        ! index_crisis_global:1 == no crisis; index_crisis_global:2 == crisis.
                        index_crisis_global = i_crisis
                        bc = bc_grid(i_crisis)
                        m_effective =  m_fun(k_initial) * ((1d+0 - bc) + bc * (1d+0 - eshock(i_e_global) * k_epsilon_frac))

                        b_initial = b_grid(1) ! I DO this just to have b_initial with a value in the default section of the code.
                        transfer_global = 0.0d+0 ! There is no bailout under default.
                        i_default_global = 2 !Country defaults
                        i_b_global = 1 !There is no borrowing in the one-period economy (== last period in the finite horizon)
                        i_b_next = 1 !There is no borrowing in the one-period economy (== last period in the finite horizon)                        

                        CALL ps_lp_and_default(consumption1,labor1,loan1, r1)

                        v1_matrix(i_k,i_y,i_e,i_crisis) = u_fun(consumption1,labor1)
                        w1_matrix(i_k,i_y,i_e,i_crisis) = r1 * loan1 + m_effective - k_grid(1)
                        vw1_matrix(i_k,i_y,i_e,i_crisis)= theta*v1_matrix(i_k,i_y,i_e,i_crisis) + &
                        (1.0d+0 - theta)* w1_matrix(i_k,i_y,i_e,i_crisis)

                        r1_matrix(i_k,i_y,i_e,i_crisis) = r1
                        labor1_matrix(i_k,i_y,i_e,i_crisis) = labor1
                        consumption1_matrix(i_k,i_y,i_e,i_crisis) = consumption1
                        loan1_matrix(i_k,i_y,i_e,i_crisis) = loan1
                        tax1_matrix(i_k,i_y,i_e,i_crisis) = tax_lp_and_default
                    END DO
                END IF
            
                value_hh_default_matrix(:,i_k , i_y, i_e) = (1d+0-pi)*v1_matrix(i_k,i_y,i_e,1) + pi *v1_matrix(i_k, i_y,i_e,2)
                value_bank_default_matrix(:,i_k, i_y, i_e) = (1d+0-pi)*w1_matrix(i_k,i_y,i_e,1) + pi *w1_matrix(i_k, i_y,i_e,2)
                value_default_matrix(:,i_k, i_y, i_e) = (1d+0-pi)*vw1_matrix(i_k,i_y,i_e,1) + pi *vw1_matrix(i_k, i_y,i_e,2)
            END DO !i_k
        END DO !i_y
    END DO !i_e

    DO i_b = 1,b_num
        b_initial = b_grid(i_b)
        ! Set b_next0_matrix to the index of the closest value on b_grid to (1-mrate)*b_initial
        b_next0_matrix(i_b,:, :, :, :) = MINLOC(ABS(b_grid - ((1.0d+0 - mrate) * b_initial)), 1)
    END DO


   DO i_e = 1,epsilon_num
        i_e_global = i_e
        A_worst_case = AA * (1d+0 - eshock(i_e_global))

        DO i_y = 1,y_num
            i_y_global=i_y
            y_initial = y_grid(i_y)

            DO i_k = 1, k_num
                i_k_global  = i_k
                k_initial = k_grid(i_k)            
      
                DO i_b = 1,b_num
                    i_b_global = i_b
                    b_initial = b_grid(i_b)
                    i_b_next = 1 !There is no borrowing in the one-period economy (== last period in the finite horizon)
                    i_default_global = 1  ! country doesn't default
         
                    CALL optimize_lp(index_opt_transfer, cons_no_bc, labor_no_bc, rate_no_bc, tax_no_bc,&
                    cons_bc, labor_bc, rate_bc, tax_bc, ik_no_bc, ik_bc)

                    DO i_crisis=1,2

                        bc = bc_grid(i_crisis)
                        
                        m_effective =  m_fun(k_initial) * ((1d+0 - bc) + bc * (1d+0 - eshock(i_e_global) * k_epsilon_frac))
                        consumption0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * cons_no_bc + bc * cons_bc 
                        labor0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * labor_no_bc + bc * labor_bc 
                        r0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * rate_no_bc + bc * rate_bc 
                        tax0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * tax_no_bc + bc * tax_bc 
                                                               
                        guarantee0_matrix(i_b, i_k, i_y,i_e,i_crisis) = &
                        min(total_damage(i_e_global,i_k_global),T_max_grid(i_k))*transfer_grid(index_opt_transfer)

                        transfer0_matrix(i_b, i_k, i_y,i_e,i_crisis) = (1d+0 - bc) * 0d+0 + &
                        bc * guarantee0_matrix(i_b, i_k, i_y,i_e,i_crisis)

                        loan0_matrix(i_b, i_k, i_y,i_e,i_crisis) = gamma_firm * alpha *&
                        y_fun(labor0_matrix(i_b, i_k, i_y,i_e,i_crisis))/&
                        (1d+0 + r0_matrix(i_b, i_k, i_y,i_e,i_crisis)*gamma_firm)
               
                        v0_matrix(i_b, i_k, i_y,i_e,i_crisis)  = u_fun(consumption0_matrix(i_b, i_k, i_y,i_e,i_crisis),&
                        labor0_matrix(i_b, i_k, i_y,i_e,i_crisis))
               
                        w0_matrix(i_b,i_k,i_y,i_e,i_crisis)  = transfer0_matrix(i_b, i_k, i_y,i_e,i_crisis) + &
                        b_initial *(mrate + (1d+0-mrate)*coupon) + &
                        loan0_matrix(i_b, i_k, i_y,i_e,i_crisis) * r0_matrix(i_b, i_k, i_y,i_e,i_crisis) +&
                        m_effective - k_grid(1)

                        vw0_matrix(i_b, i_k, i_y,i_e,i_crisis) = theta*v0_matrix(i_b,i_k,i_y,i_e,i_crisis) + &
                        (1.0d+0 - theta)* w0_matrix(i_b,i_k,i_y,i_e,i_crisis)

                    END DO ! i_crisis

                    value_hh_repay_matrix(i_b,i_k,i_y,i_e) = (1d+0-pi)*v0_matrix(i_b,i_k,i_y,i_e,1) + &
                    pi *v0_matrix(i_b,i_k,i_y,i_e,2)
                    value_bank_repay_matrix(i_b,i_k,i_y,i_e) = (1d+0-pi)*w0_matrix(i_b,i_k,i_y,i_e,1) + &
                    pi *w0_matrix(i_b,i_k,i_y,i_e,2)
                    value_repay_matrix(i_b,i_k,i_y,i_e) = (1d+0-pi)*vw0_matrix(i_b,i_k,i_y,i_e,1) + &
                    pi *vw0_matrix(i_b,i_k,i_y,i_e,2)

                    IF (value_default_matrix(i_b,i_k,i_y,i_e) > value_repay_matrix(i_b,i_k,i_y,i_e)) THEN
                       default_decision_new(i_b,i_k,i_y,i_e) = 2
                       value_matrix(i_b,i_k,i_y,i_e) = value_default_matrix(i_b,i_k,i_y,i_e)
                       value_hh_matrix(i_b,i_k,i_y,i_e) = value_hh_default_matrix(i_b,i_k,i_y,i_e)
                       value_bank_matrix(i_b,i_k,i_y,i_e) = value_bank_default_matrix(i_b,i_k,i_y,i_e)
                    ELSE
                       default_decision_new(i_b,i_k,i_y,i_e) = 1
                       value_matrix(i_b,i_k,i_y,i_e) = value_repay_matrix(i_b,i_k,i_y,i_e)
                       value_hh_matrix(i_b,i_k,i_y,i_e) = value_hh_repay_matrix(i_b,i_k,i_y,i_e)
                       value_bank_matrix(i_b,i_k,i_y,i_e) = value_bank_repay_matrix(i_b,i_k,i_y,i_e)
                    END IF

                END DO ! i_b
            END DO ! i_k
        END DO ! i_y
    END DO ! i_e

    default_decision=default_decision_new

ELSE !READ DATA FROM EXTERNAL FILES
    ! OPEN (5, FILE='graphs_iter_dss.txt')
    OPEN (7, FILE='graphs_value_planner_dss.txt')
    OPEN (8, FILE='graphs_value_hh_dss.txt')
    OPEN (9, FILE='graphs_value_bank_dss.txt')
    OPEN (10, FILE='graphs_v_dss.txt')
    OPEN (16, FILE='graphs_q.txt')
    OPEN (19, FILE='graphs_b_next.txt')
    OPEN (21, FILE='graphs_taxes.txt')
    OPEN (22, FILE='graphs_labor.txt')
    OPEN (23, FILE='graphs_transfers.txt')
    OPEN (24, FILE='graphs_guarantees.txt')
    OPEN (25, FILE='graphs_interest.txt')
    OPEN (26, FILE='graphs_vw_dss.txt')
    OPEN (27, FILE='graphs_w_dss.txt')
    OPEN (28, FILE='graphs_loans.txt')
    OPEN (29, FILE='graphs_consumption.txt')
    OPEN (30, FILE='graphs_cons_bank.txt')
    OPEN (31, FILE='graphs_k_next.txt')
   
   ! READ (5,'(I3)') iter
   
    DO i_crisis=1,2
    DO i_e = 1,epsilon_num
    DO i_y = 1,y_num
    DO i_k = 1,k_num
    DO i_b = 1,b_num

        READ(7, *)  value_matrix(i_b,i_k,i_y,i_e), value_repay_matrix(i_b,i_k,i_y,i_e), value_default_matrix(i_b,i_k,i_y,i_e)
        READ(8, *)  value_hh_matrix(i_b,i_k,i_y,i_e), value_hh_repay_matrix(i_b,i_k,i_y,i_e), &
        value_hh_default_matrix(i_b,i_k,i_y,i_e)
        READ(9, *)  value_bank_matrix(i_b,i_k,i_y,i_e), value_bank_repay_matrix(i_b,i_k,i_y,i_e), &
        value_bank_default_matrix(i_b,i_k,i_y,i_e)
        READ(10, *) v_matrix(i_b, i_k, i_y, i_e, i_crisis) , v0_matrix(i_b, i_k, i_y, i_e, i_crisis),&
        v1_matrix(i_k, i_y, i_e, i_crisis)
        READ(16, *) q_vector(i_b,i_k, i_y), q_matrix_nodef(i_b, i_k, i_y, i_e, i_crisis)
        READ(19, *) b_next_matrix(i_b, i_k, i_y, i_e, i_crisis), b_next0_matrix(i_b, i_k, i_y, i_e, i_crisis)
        READ(25, *) r_matrix(i_b, i_k, i_y, i_e, i_crisis), r0_matrix(i_b, i_k, i_y, i_e, i_crisis),&
        r1_matrix(i_k, i_y, i_e,i_crisis)
        READ(26, *) vw_matrix(i_b, i_k, i_y, i_e, i_crisis), vw0_matrix(i_b, i_k, i_y, i_e, i_crisis),&
        vw1_matrix(i_k, i_y, i_e, i_crisis)
        READ(27, *) w_matrix(i_b, i_k, i_y, i_e, i_crisis) , w0_matrix(i_b, i_k, i_y, i_e, i_crisis), &
        w1_matrix(i_k, i_y, i_e, i_crisis)
        READ (28,*) loan_matrix(i_b,i_k,i_y, i_e, i_crisis), loan0_matrix(i_b,i_k,i_y, i_e, i_crisis), &
        loan1_matrix(i_k, i_y, i_e, i_crisis)
        READ(29,*) consumption_matrix(i_b,i_k,i_y, i_e, i_crisis), consumption0_matrix(i_b,i_k,i_y, i_e, i_crisis), &
        consumption1_matrix(i_k, i_y, i_e, i_crisis)
        
        READ(30,*) cons_bank_matrix(i_b,i_k,i_y, i_e, i_crisis), cons_bank0_matrix(i_b,i_k,i_y, i_e, i_crisis), &
            cons_bank1_matrix(i_k, i_y, i_e, i_crisis)

        READ (21, *) tax_matrix(i_b,i_k,i_y, i_e, i_crisis), tax0_matrix(i_b,i_k,i_y, i_e, i_crisis), &
            tax1_matrix(i_k,i_y, i_e, i_crisis)

        READ (22, *) n_matrix(i_b,i_k,i_y, i_e, i_crisis), labor0_matrix(i_b,i_k,i_y, i_e, i_crisis),&
        labor1_matrix(i_k,i_y, i_e, i_crisis)
        
        READ (23, *) transfer_matrix(i_b,i_k,i_y, i_e, i_crisis), transfer0_matrix(i_b,i_k,i_y, i_e, i_crisis),&
        transfer1_matrix(i_k,i_y,i_e,i_crisis)

        READ (24, *) guarantee_matrix(i_b,i_k,i_y, i_e, i_crisis), guarantee0_matrix(i_b,i_k,i_y, i_e, i_crisis),&
        guarantee1_matrix(i_k, i_y,i_e,i_crisis)

        READ(31,*) k_next_matrix(i_b,i_k,i_y,i_e,i_crisis), dummy_1, dummy_2

        IF (value_repay_matrix(i_b,i_k,i_y,i_e)< value_default_matrix(i_b,i_k,i_y,i_e)) THEN
            default_decision(i_b,i_k,i_y,i_e) =2
        ELSE
            default_decision(i_b,i_k,i_y,i_e) =1
        END IF

    END DO ! i_b
    END DO ! i_k
    END DO ! i_y
    END DO ! i_e
    END DO ! i_crisis

    ! CLOSE (5)
    CLOSE (7)
    CLOSE (8)
    CLOSE (9)
    CLOSE(10)
    CLOSE(16)
    CLOSE (19)
    CLOSE (21)
    CLOSE (22)
    CLOSE (23)
    CLOSE (24)
    CLOSE (25)
    CLOSE (26)
    CLOSE (27)
    CLOSE (28)
    CLOSE (29)
    CLOSE (30)                
    CLOSE (31)
END IF !(indicator_external < 0.5d+0)

IF (simulonly.EQ.0) THEN
    PRINT *, 'Solve model...'
    CALL iterate
END IF
IF (runwelf.EQ.1) CALL welfar
PRINT *, 'Run Simulations...'
CALL simulate

PRINT *, 'Results printing...'
501 FORMAT (I8, 36f15.10, I8, 4f15.10)
OPEN (5,FILE=TRIM(path)//'/results.out',FORM="FORMATTED")
WRITE(5,501) num, beta, AA, sig_e, gov_spending, T_max_frac,     &
    avg_loan_to_y*100.0d+0, avg_transfer_to_y*100.0d+0, def_prob_unconditional*100.0d+0, &
    def_prob_conditional*100.0d+0, avg_g_to_y*100.0d+0, avg_std_log_y*100.0d+0, avg_b_to_y*100.0d+0, &
    avg_k_to_y*100.0d+0, avg_welf*100.0d+0, avg_welf_bk(1:20,i_k_simu)*100.0d+0, &
    deviation, dev_q, iteration, &
    avg_spread*100.0d+0, avg_std_spread*100.0d+0, avg_corr_spread_y, avg_rr*100.0d+0
CLOSE (5)

OPEN (7,FILE=TRIM(path)//'/results_storage.out',FORM="FORMATTED")
WRITE(7,*) AA, alpha_k, k_epsilon_frac, scale_mk, beta,  &
    avg_loan_to_y*100.0d+0, avg_transfer_to_y*100.0d+0, def_prob_unconditional*100.0d+0, &
    def_prob_conditional*100.0d+0, avg_g_to_y*100.0d+0, avg_std_log_y*100.0d+0, &
    avg_b_to_y*100.0d+0, avg_k_to_y*100.0d+0, avg_welf*100.0d+0,&
    avg_spread*100.0d+0, avg_std_spread*100.0d+0, avg_corr_spread_y,&
    avg_rr*100.0d+0, deviation, dev_q, iteration,&
    avg_exposure_1*100.0d+0, avg_exposure_2*100.0d+0, avg_sum_of_A*100.0d+0,&
    avg_frac_endog_A*100.0d+0, avg_y, avg_k_to_assets*100.0d+0
CLOSE (7)

CALL cpu_time(end_time)
WRITE(nout, '(A7, X, A7, X, A7)') 'Hours ', 'Minutes', 'Seconds'
WRITE(nout, '(I7, X, I7, X, I7)') INT((end_time - start_time) / 3600d+0), &
INT((end_time-start_time)/60d+0 - INT((end_time - start_time) / 3600d+0)*60d+0),&
INT(end_time-start_time - INT((end_time - start_time) / 3600d+0)*3600d+0 - &
INT((end_time-start_time)/60d+0 - INT((end_time - start_time) / 3600d+0)*60d+0)*60d+0)

CONTAINS
    
FUNCTION ind2sub(iG,nSub) RESULT(iSub)
!! Compute the indices in each dimension from the global index
!====================================================================!
  INTEGER, INTENT(IN)	:: iG !! Index into a global vector
  INTEGER, INTENT(IN) 	:: nSub(:) !! Size in each dimension
  INTEGER 				:: iSub(SIZE(nSub)) !! Indices in each dimension to return
  INTEGER 				:: i,iGtmp,iTmp
  INTEGER 				:: nDims
  INTEGER 				:: prod

  nDims=SIZE(nSub)
  IF (nDims == 1) THEN
    iSub(1) = iG
    RETURN
  END IF

  prod = PRODUCT(nSub)
  iGtmp = iG
  DO i = nDims,1,-1
    prod = prod / nSub(i)
    iTmp = MOD(iGtmp-1,prod)+1
    iSub(i) = (iGtmp - iTmp)/prod + 1
    iGtmp = iTmp
  END DO

END FUNCTION
    
END PROGRAM main


SUBROUTINE welfar
    USE param
    IMPLICIT NONE
    INTEGER:: iter, idx_y, idx_e, idx_i, idx_b, idx_bp, idx_yp, idx_ep, ceq_num, &
        idx_crisis, point, idx_b_next, itermax, idx_k, idx_k_next
    DOUBLE PRECISION :: ceq_low, ceq_high
    PARAMETER (ceq_num=10, ceq_low=-0.95d+0, ceq_high=2.0d+0)
    DOUBLE PRECISION :: con, hour, evp, evp1, dum, cons_banker
    DOUBLE PRECISION, DIMENSION (:,:,:,:,:), ALLOCATABLE :: vhati,vhati_old
    INTEGER, DIMENSION (b_num,k_num,y_num,epsilon_num,2) :: bnext_nb, bnext0_nb
    INTEGER, DIMENSION (b_num,k_num,y_num,epsilon_num,2) :: knext_nb, knext0_nb, knext1_nb
    DOUBLE PRECISION, DIMENSION (b_num,k_num,y_num,epsilon_num,2) :: labor_nb, cons_nb, bank_nb
    DOUBLE PRECISION, DIMENSION (b_num,k_num,y_num,epsilon_num,2) :: labor0_nb, cons0_nb, bank0_nb
    DOUBLE PRECISION, DIMENSION (k_num, y_num,epsilon_num,2) :: labor1_nb, cons1_nb, bank1_nb
    DOUBLE PRECISION, DIMENSION (b_num,k_num,y_num,epsilon_num) :: def_nb
    DOUBLE PRECISION, DIMENSION (k_num, y_num,epsilon_num) :: vdeftemp
    DOUBLE PRECISION, DIMENSION (ceq_num) :: ceq_grid
    DOUBLE PRECISION, EXTERNAL :: u_fun, interp1, zbrent

    ALLOCATE(vhati(b_num,k_num,y_num,epsilon_num,ceq_num))
    ALLOCATE(vhati_old(b_num,k_num,y_num,epsilon_num,ceq_num))

    IF (ceq_num>1) THEN
        CALL creategrid(ceq_grid, ceq_num, ceq_low, 0.0d+0, ceq_high, 2.0d+0, 1.5d+0)
        !PRINT *, 'ceq grid:', ceq_grid
    END IF

    OPEN (11, FILE='nb_files/graphs_default_dss.txt')
    OPEN (19, FILE='nb_files/graphs_b_next.txt')
    OPEN (22, FILE='nb_files/graphs_labor.txt')
    OPEN (29, FILE='nb_files/graphs_consumption.txt')
    OPEN (30, FILE='nb_files/graphs_cons_bank.txt')
    OPEN (31, FILE='nb_files/graphs_k_next.txt')
    
    DO idx_crisis=1,2
    DO idx_e = 1,epsilon_num
    DO idx_y = 1,y_num
    DO idx_k = 1,k_num
    DO idx_b = 1,b_num
        !IF (idx_crisis.EQ.1) 
        READ(11,*) def_nb(idx_b,idx_k, idx_y, idx_e)
        READ(19,*) bnext_nb(idx_b, idx_k, idx_y, idx_e, idx_crisis), bnext0_nb(idx_b, idx_k, idx_y, idx_e, idx_crisis)
        READ(22,*) labor_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis), labor0_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis), &
            labor1_nb(idx_k, idx_y, idx_e, idx_crisis)
        READ(29,*) cons_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis), cons0_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis),   &
            cons1_nb(idx_k, idx_y, idx_e, idx_crisis)
        READ(30,*) bank_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis), bank0_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis),   &
            bank1_nb(idx_k, idx_y, idx_e, idx_crisis)
        READ(31,*) knext_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis), knext0_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis),   &
            knext1_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis)
    END DO
    END DO
    END DO
    END DO
    END DO

    CLOSE(11)
    CLOSE(19)
    CLOSE(22)
    CLOSE(29)
    CLOSE(30)
    CLOSE(31)

    vhati = 0.0d+0
    IF (indicator_external>0.5d+0) THEN
        IF (indicator_bailout_default>0.5d+0) THEN
            itermax = 98
        ELSE
            itermax = 48
        ENDIF
        
    ELSE
        itermax = iteration
    ENDIF
    PRINT *, 'iterations:', itermax

    DO idx_i = 1, ceq_num
        vdeftemp = 0.0d+0
        DO iter = 1, itermax
            vhati_old(:,:,:,:,idx_i) = vhati(:,:,:,:,idx_i)
            DO idx_k= 1,k_num            
                DO idx_y = 1, y_num
                    DO idx_e = 1, epsilon_num

                        ! Default/Excluded
                        DO idx_crisis = 1,2
                            con = ( 1.0d+0 + ceq_grid(idx_i) )*cons1_nb(idx_k, idx_y, idx_e, idx_crisis)
                            cons_banker = bank1_nb(idx_k, idx_y, idx_e, idx_crisis)
                            hour = labor1_nb(idx_k, idx_y, idx_e, idx_crisis)
                            evp = theta * u_fun(con,hour) &
                                + (1d+0-theta) * cons_banker
                            idx_k_next = knext1_nb(1,idx_k, idx_y, idx_e, idx_crisis)

                            DO idx_yp = 1,y_num
                                DO idx_ep = 1, epsilon_num
                                    evp = evp + beta*trans_matrix(idx_y, idx_yp)*petran(idx_ep) &
                                        *(prob_excl_end*vhati_old(1,idx_k_next,idx_yp, idx_ep, idx_i)      &
                                        +(1.0d+0-prob_excl_end)*vdeftemp(idx_k_next, idx_yp, idx_ep))
                                END DO
                            END DO
                            IF (idx_crisis.EQ.1) THEN
                                evp1 = evp
                            END IF  
                        END DO
                        vdeftemp(idx_k, idx_y, idx_e) = (1.0d+0 - pi)*evp1 + pi*evp

                        ! Repay
                        DO idx_b = 1, b_num
                            DO idx_crisis = 1,2
                                con = ( 1.0d+0 + ceq_grid(idx_i) )*cons0_nb(idx_b,idx_k, idx_y, idx_e, idx_crisis)
                                cons_banker = bank0_nb(idx_b, idx_k, idx_y, idx_e, idx_crisis)
                                hour = labor0_nb(idx_b, idx_k, idx_y, idx_e, idx_crisis)
                                idx_b_next = bnext0_nb(idx_b, idx_k, idx_y, idx_e, idx_crisis)
                                idx_k_next = knext0_nb(idx_b, idx_k, idx_y, idx_e, idx_crisis)
                            
                                evp = theta * u_fun(con,hour)   &
                                    + (1d+0-theta) * cons_banker
                                DO idx_yp = 1,y_num
                                    DO idx_ep = 1, epsilon_num
                                        !interp = interp1(b_grid,func,b_num,b_grid(idx_b_next))
                                        !evp = evp + beta*trans_matrix(idx_y, idx_yp)*petran(idx_ep) &
                                        !    *interp1(b_grid,vhati_old(: ,idx_yp, idx_ep, idx_i),b_num,b_grid   (idx_b_next))
                                        evp = evp + beta*trans_matrix(idx_y, idx_yp)*petran(idx_ep) &
                                            *vhati_old(idx_b_next,idx_k_next,idx_yp, idx_ep, idx_i)
                                    END DO
                                END DO
                                IF (idx_crisis.EQ.1) THEN
                                    evp1 = evp
                                ENDIF                        
                            END DO                        

                            IF (def_nb(idx_b,idx_k, idx_y, idx_e)>0.5d+0) THEN
                                vhati(idx_b,idx_k, idx_y, idx_e, idx_i) = vdeftemp(idx_k, idx_y, idx_e)
                            ELSE
                                vhati(idx_b,idx_k, idx_y, idx_e, idx_i) = (1.0d+0 - pi)*evp1 + pi*evp
                            ENDIF

                        END DO                
                    END DO
                END DO
            END DO  
        END DO
    END DO


    IF (writeout.EQ.1) THEN
        OPEN (unit=9, file="graphs_simulation_vhati.txt", position="rewind")
        WRITE (9, *) vhati
        CLOSE (unit=9)
    ENDIF

    PRINT *, "computing welfare"

   

    DO idx_b=1,b_num
    DO idx_k=1,k_num
    DO idx_y = 1,y_num
    DO idx_e = 1, epsilon_num
        IF ((vhati(idx_b,idx_k, idx_y, idx_e, 1)-value_matrix(idx_b, idx_k, idx_y, idx_e)) &
            *(vhati(idx_b, idx_k, idx_y, idx_e, ceq_num)-value_matrix(idx_b, idx_k, idx_y, idx_e))<0.0d+0)THEN
            welfares(idx_b,idx_k, idx_y, idx_e)= zbrent(WELF, ceq_low, ceq_high, 1.0D-14)
        ELSE
            !PRINT *, 'Root not bracketed!'
            PRINT *, 'idx_b:', idx_b
            PRINT *, 'idx_k:', idx_k
            PRINT *, 'idx_y:', idx_y
            PRINT *, 'idx_e:', idx_e
            ERROR STOP 'Root not bracketed!'
        ENDIF
    END DO
    END DO
    END DO
    END DO

    avg_welf_bk = zero
    DO idx_b = 1,b_num
        do idx_k = 1,k_num            
            DO idx_y = 1,y_num
                DO idx_e = 1, epsilon_num
                    avg_welf_bk(idx_b, idx_k) = avg_welf_bk(idx_b, idx_k) + &
                    welfares(idx_b, idx_k, idx_y, idx_e)*Erd(idx_y)*petran(idx_e)
                END DO
            END DO
        END DO
    END DO
    
    WRITE(*,*) 'WELF GAIN rel. to No Bailout (keeping k near steady state), percent)'
    WRITE(*,*) 'B=0, ', 100d+0 * avg_welf_bk(1,10)
    WRITE(*,*) 'B=', b_grid(2), ' ', 100d+0 * avg_welf_bk(2,10)
    WRITE(*,*) 'B=', b_grid(4), ' ', 100d+0 * avg_welf_bk(4,10)
    WRITE(*,*) 'B=', b_grid(8), ' ', 100d+0 * avg_welf_bk(8,10)
    WRITE(*,*) 'B=', b_grid(12), ' ', 100d+0 * avg_welf_bk(12,10)

    IF (writeout.EQ.1) THEN
        OPEN (unit=9, file="graphs_welfare.txt", position="rewind")
        WRITE (9, *) welfares
        CLOSE (unit=9)
    ENDIF

    DEALLOCATE(vhati)
    DEALLOCATE(vhati_old)

    CONTAINS
    
    DOUBLE PRECISION FUNCTION WELF(ceq)
    ! LOCAL VARIABLES USED WITHIN LOOPS
    DOUBLE PRECISION, INTENT(IN) :: ceq
    DOUBLE PRECISION             :: vc, tmp_vec(ceq_num)
    
    tmp_vec = vhati(idx_b, idx_k, idx_y, idx_e, :)

    vc = interp1(ceq_grid, tmp_vec, ceq_num, ceq)
    WELF = vc - value_matrix(idx_b, idx_k, idx_y, idx_e)

    END FUNCTION WELF

END SUBROUTINE welfar
    
SUBROUTINE simulate
    USE param
    IMPLICIT NONE
    INTEGER :: period_num, period_start, i,j,sample_num, default_num, i_y_previous, i_y_current, i_b_current, &
    i_e_current, i_crisis_current, counter_def_conditional, sample_length, counter_no_def, counter_bc_no_def, index, index_2, &
    idx_y, idx_e, idx_b, i_k_current 
    PARAMETER (sample_num =500, period_num=1501, period_start=51)
    INTEGER, DIMENSION (period_num,sample_num) :: dd, excl, bc_crisis, y_dev, y_trend
    DOUBLE PRECISION :: random_matrix(period_num, sample_num, 4), random_vector(1:4*sample_num*period_num), &
    b(period_num+1,sample_num), nodef_check, sum_defaults, sum_bc, &!avg_cons,   &
    avg_tr, sample_std_log_y(sample_num), k(period_num+1,sample_num), m_effective
    DOUBLE PRECISION, DIMENSION (period_num,sample_num) ::z, q , c, y, tax, nn, pi_firm,cons_bank,rr,loan,  tax_repayment, &
    wage, trans, guarantee, spread_ss, tr_y_elasticity, tr_y_multiplier, ytm_bond, ytm_free, spread_alt, exposure_1, exposure_2,&
    sum_of_A, Frac_endog_A, M_k

    
    DOUBLE PRECISION, EXTERNAL :: q_fun, objective_function, r_fun, q_fun_nodef, rloan_next, m_fun

    !!!!!!! NOTE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! z DENOTE UNDERLYING SHOCK TO THE ENDOWMENT
    ! y DENOTE REALIZED ENDOWMENT (EXP(z))
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    CALL init_random_seed()
    CALL RANDOM_NUMBER(random_vector)

    DO j=1,sample_num
        DO i=1,period_num
          random_matrix(i,j,1)=random_vector((j-1)*period_num+i) !Use for TFP shocks
          random_matrix(i,j,2)=random_vector(period_num*sample_num + (j-1)*period_num+i) !Use for epsilon shocks
          random_matrix(i,j,3)=random_vector(2*period_num*sample_num + (j-1)*period_num+i) !Use for pi shocks
          random_matrix(i,j,4)=random_vector(3*period_num*sample_num + (j-1)*period_num+i) ! Use for reentry
        END DO
    END DO

    IF (writeout.EQ.1) THEN
        OPEN (UNIT=31, FILE="graphs_data_sim_dss.txt", status = 'replace')
        OPEN (UNIT=32, FILE="graphs_def_per_dss.txt", status = 'replace')
        OPEN (UNIT=33, FILE="graphs_param_simulation_dss.txt", status = 'replace')

        ! OPEN (UNIT=37, FILE="graphs_moment.txt", status = 'replace')
        ! OPEN (UNIT=38, FILE="graphs_data_sim_2_dss.txt", status = 'replace')

        WRITE(33, '(I10)') period_num-1
        WRITE(33, '(I10)') sample_num
        CLOSE(33)
    END IF

    i_default_global = 1 ! Start with no default (position: 1= no default, 2= default)
    tr_y_elasticity = -999d+0 ! Initialize elasticity to NaN
    tr_y_multiplier = -999d+0 ! Initialize elasticity to NaN

    DO j=1,sample_num   !SOLVE FOR SAMPLE j
        IF(multpl(j,100) == 1) THEN 
             WRITE(nout, *)'sample', j
        ENDIF

        i_y_previous = 13
        z(1,j) = y_grid(i_y_previous)
        y(1,j) = EXP(z(1,j))
        b(1,j) = zero
        k(1,j) = zero

        i_b_next = 1                  !INDEX OF BOND POSITION IN THE NEXT PERIOD.
        b(2,j) = b_grid(1)
        i_k_next = 1                  !INDEX OF CAPITAL POSITION IN THE NEXT PERIOD.
        k(2,j) = k_grid(1)

        dd(1,j) = 1    !NO DEFAULT IN FIRST PERIOD
        excl(1,j) = 1  ! NO EXCLUSION IN THE FIRST PERIOD
        default_num = 0d+0  !variable that counts the number of defaults in each sample.
        !bad_epsilon(1,j) = 0    !NO epsilon_shock in the first period
        bc_crisis(1,j) = 0      !NO banking crisis in the first period
        loan(1,j)=zero
        tax_repayment(1,j)=zero
        !welf_sim(1,j) = welfares(1,i_y_previous,1)

        DO i=2,period_num
            !epsilon = realization of standard gaussian * standard deviation
            i_y_current = 1
            DO WHILE(cdf_matrix(i_y_previous, i_y_current) < random_matrix(i,j,1))
               i_y_current = i_y_current + 1
            END DO

            i_e_current = 1      
            DO WHILE(cum_petran(i_e_current) < random_matrix(i,j,2))
               i_e_current = i_e_current + 1
            END DO
            A_worst_case = AA * (1d+0 - eshock(i_e_current))

            !IF (i_e_current > 1) THEN
            !  bad_epsilon(i,j) = 1
            !ELSE
            !  bad_epsilon(i,j) = 0
            !END IF

            bc_crisis(i,j) = 0 !set it to zero ex-ante

            IF (random_matrix(i,j,3) < pi) THEN
                i_crisis_current = 2 !pi-shock hits
                
                IF (i_e_current > 1) bc_crisis(i,j) = 1

            ELSE
                i_crisis_current = 1 !pi-shock doesn't hit
            END IF
      
            z(i,j) = y_grid(i_y_current)
            i_y_previous = i_y_current
            i_b_current = i_b_next  !CURRENT INDEX OF ASSET = PREVIOUS INDEX OF SAVING
            i_k_current = i_k_next  !CURRENT INDEX OF ASSET = PREVIOUS INDEX OF SAVING
            y_initial = z(i,j)
            b_initial = b(i,j)
            k_initial = k(i,j)

            exposure_1(i,j) = b_initial/(AA + m_fun(k_initial)+b_initial)
            exposure_2(i,j) = b_initial/(AA + k_initial+b_initial)
            sum_of_A(i,j) = AA + m_fun(k_initial)
            Frac_endog_A(i,j) = m_fun(k_initial)/(AA + m_fun(k_initial))
            M_k(i,j) = m_fun(k_initial)

            !welf_sim(i,j) = welfares(i_b_current, i_y_current, i_e_current)

            IF (excl(i-1,j) ==2 ) THEN  !COUNTRY WAS EXCLUDED IN THE PREVIOUS PERIOD
                IF (random_matrix(i,j,4) < prob_excl_end) THEN  ! EXCLUSION ENDS THIS PERIOD
                    
                    dd(i,j) = 1   !COUNTRY DOES NOT DEFAULT
                    excl(i,j) = 1
                    i_default_global = dd(i,j)  
                    i_b_next = b_next_matrix(i_b_current,i_k_current, i_y_current, i_e_current, i_crisis_current) 
                    i_k_next = k_next_matrix(i_b_current,i_k_current, i_y_current, i_e_current, i_crisis_current)
                    b(i+1,j) = b_grid(i_b_next)
                    k(i+1,j) = k_grid(i_k_next)
                    q(i,j)   = q_fun(i_b_next, i_k_next, i_y_current)  !q_paid -- Can probably just READ it.
                    !spread_ss(i,j) =(1/q(i,j))-(1/q_fun_nodef(i_b_next, i_y_current))
                    !spread_ss(i,j) =(1d+0/(q(i,j)*(1+rf_rate))-1d+0)*(mrate+rf_rate)
                    ytm_bond(i,j)= mrate*((1d+0/q(i,j))-1d+0)+(1d+0-mrate)*coupon/q(i,j)
                    ytm_free(i,j) =rloan_next(i_b_next,i_k_next, i_y_current)*(1d+0+rf_rate)-1d+0
                    spread_ss(i,j) = ytm_bond(i,j)-ytm_free(i,j)
                    spread_alt(i,j) = 1d+0 - (q(i,j)/((mrate + (1d+0-mrate)*coupon)/(rf_rate + mrate)))
                    tax(i,j) = tax0_matrix(i_b_current, i_k_current, i_y_current, i_e_current,    i_crisis_current)
                    nn(i,j) = labor0_matrix(i_b_current, i_k_current, i_y_current, i_e_current,   i_crisis_current)
                    y(i,j) = EXP(z(i,j))*nn(i,j)**(alpha)

                    trans(i,j)= transfer0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                    guarantee(i,j)=guarantee0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                    transfer_global = trans(i,j)                                                                                

                    IF (trans(i,j)>0d+0) THEN
                        tr_y_multiplier(i,j) = (output_mat_opt_transfer(i_b_current, i_k_current, i_y_current,   i_e_current) &
                        - output_mat_no_transfer(i_b_current, i_k_current, i_y_current, i_e_current))/trans(i,j)
                        !tr_y_elasticity(i,j) = (output_mat_opt_transfer(i_b_current, i_y_current,    i_e_current) &
                        !    - output_mat_no_transfer(i_b_current, i_y_current, i_e_current))/ &
                        !    (output_mat_opt_transfer(i_b_current, i_y_current, i_e_current) &
                        !    + output_mat_no_transfer(i_b_current, i_y_current, i_e_current))
                    END IF

                    pi_firm(i,j) = (1d+0-alpha)* y(i,j)
                    ! rr(i,j) = r_fun(nn(i,j)) ! requires b_initial, i_default_global, transfer_global
                    rr(i,j) = r0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                    loan(i,j)= loan0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                
                    m_effective =  m_fun(k_initial) * ((1d+0 - bc_grid(i_crisis_current)) +&
                    bc_grid(i_crisis_current) * (1d+0 - eshock(i_e_current) * k_epsilon_frac))

                    cons_bank(i,j) = trans(i,j) + loan(i,j) * rr(i,j) + &
                    b_initial*(mrate+(1d+0-mrate)*coupon) -  (b(i+1,j)-(1d+0-mrate)*b_initial)*q(i,j) + &
                    m_effective  - k_grid(i_k_next)
                    c(i,j) = consumption0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                    wage(i,j)= EXP(z(i,j))*alpha*nn(i,j)**(alpha-1d+0)/(1d+0 + gamma_firm*rr(i,j))
                    !wage_repayment(i,j)=wage(i,j)
                    !nn_repayment(i,j)=nn(i,j)
                    tax_repayment(i,j) = tax(i,j)

                ELSE ! EXCLUSION DOES NOT END THIS PERIOD

                    dd(i,j) = 1   !COUNTRY DOES NOT DEFAULT
                    excl(i,j) = 2
                    i_default_global = 2 ! here it would usually be dd(i,j), but I use 2 just to ensure   that    when   using r_fun I use the correct thing!!
                    i_b_next = 1 ! the country cannot borrow becuase it is exluced
                    i_k_next = k_next_matrix(i_b_current,i_k_current, i_y_current, i_e_current, i_crisis_current)
                    b(i+1,j) = b_grid(i_b_next)
                    k(i+1,j) = k_grid(i_k_next)
                    q(i,j) = 0.0d+0 !q_fun(i_b_next, i_y_current)  !q_paid
                    spread_ss(i,j) = 999d+0 !(1/q(i,j))-(1/q_fun_nodef(i_b_next, i_y_current))
                    spread_alt(i,j) = 999d+0
                    tax(i,j) = tax1_matrix(i_k_current,i_y_current, i_e_current, i_crisis_current)
                    nn(i,j)  = labor1_matrix(i_k_current,i_y_current, i_e_current, i_crisis_current)
                    y(i,j) = EXP(z(i,j))*nn(i,j)**(alpha)

                    IF (indicator_bailout_default > 0.5d+0) THEN
                        trans(i,j) = transfer1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                        guarantee(i,j) = guarantee1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                    ELSE
                        trans(i,j) = 0d+0 !A government excluded cannot make transfers
                        guarantee(i,j) = 0d+0 !A government excluded cannot make transfers
                    END IF
                    
                    transfer_global = trans(i,j)
                    pi_firm(i,j) = (1d+0-alpha)* y(i,j)
                    !rr(i,j) = r_fun(nn(i,j)) ! requires b_initial, i_default_global, transfer_global
                    rr(i,j) = r1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                    loan(i,j)= loan1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                    
                    m_effective =  m_fun(k_initial) * ((1d+0 - bc_grid(i_crisis_current)) +&
                    bc_grid(i_crisis_current) * (1d+0 - eshock(i_e_current) * k_epsilon_frac))
                    cons_bank(i,j) = loan(i,j) * rr(i,j) + trans(i,j) +m_effective - k_grid(i_k_next)
                    c(i,j) = consumption1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)                    
                    wage(i,j)= EXP(z(i,j))*alpha*nn(i,j)**(alpha-1d+0)/(1d+0 + gamma_firm*rr(i,j))
                    !wage_repayment(i,j)=wage(i,j)
                    !nn_repayment(i,j)=nn(i,j)
                    tax_repayment(i,j) = tax0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)

                END IF

            ELSE ! THE COUNTRY WAS NOT EXCLUDED THE PREVIOUS PERIOD

                IF (default_decision(i_b_current, i_k_current, i_y_current, i_e_current) > 1) THEN      
                    
                    !COUNTRY DEFAULTS
                    dd(i,j) = 2
                    excl(i,j) = 2
                    i_default_global = dd(i,j)
                    i_b_next = 1
                    i_k_next = k_next_matrix(i_b_current,i_k_current, i_y_current, i_e_current, i_crisis_current)
                    b(i+1,j) = b_grid(i_b_next)
                    k(i+1,j) = k_grid(i_k_next)

                    q(i,j) = 0d+0!q_fun(i_b_next, i_y_current)  !q_paid
                    spread_ss(i,j) = 999d+0!(1/q(i,j))-(1/q_fun_nodef(i_b_next, i_y_current))
                    spread_alt(i,j) = 999d+0
                    tax(i,j) = tax1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                    nn(i,j) = labor1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                    y(i,j) = EXP(z(i,j))*nn(i,j)**(alpha)

                    if (indicator_bailout_default > 0.5d+0) then
                        trans(i,j) = transfer1_matrix( i_k_current, i_y_current, i_e_current, i_crisis_current)
                        guarantee(i,j) = guarantee1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                    else
                        trans(i,j) = 0d+0 !A government excluded cannot make transfers
                        guarantee(i,j) = 0d+0 !A government excluded cannot make transfers
                    end if
                    transfer_global = trans(i,j)

                    pi_firm(i,j) = (1d+0-alpha)* y(i,j)
                    !rr(i,j) = r_fun(nn(i,j)) ! requires b_initial, i_default_global, transfer_global
                    rr(i,j) = r1_matrix(i_k_current,i_y_current, i_e_current, i_crisis_current)
                    loan(i,j)= loan1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)

                    cons_bank(i,j) = loan(i,j) * rr(i,j) + trans(i,j)
                    c(i,j) = consumption1_matrix(i_k_current, i_y_current, i_e_current, i_crisis_current)
                    wage(i,j)= EXP(z(i,j))*alpha*nn(i,j)**(alpha-1d+0)/(1d+0 + gamma_firm*rr(i,j))

                    
                    tax_repayment(i,j) = tax0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)

                    IF (writeout.EQ.1) THEN
                        WRITE(32, '(I7)') i-1 !NEED TO SUBSTRACT 1. REASON: files start saving data on period 2
                    ENDIF
                    default_num = default_num + 1
                ELSE
                    !COUNTRY DOES NOT DEFAULT
                    dd(i,j) = 1   
                    excl(i,j) = 1
                    i_default_global = dd(i,j)

                    i_b_next = b_next_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)               
                    b(i+1,j) = b_grid(i_b_next)
                    i_k_next = k_next_matrix(i_b_current,i_k_current, i_y_current, i_e_current, i_crisis_current)
                    k(i+1,j) = k_grid(i_k_next)
                    q(i,j) = q_fun(i_b_next, i_k_next, i_y_current)  !q_paid

                   

                    ytm_bond(i,j)= mrate*((1d+0/q(i,j))-1d+0)+(1d+0-mrate)*coupon/q(i,j)
                    ytm_free(i,j) =rloan_next(i_b_next, i_k_next, i_y_current)*(1d+0+rf_rate)-1d+0
                    spread_ss(i,j) = ytm_bond(i,j)-ytm_free(i,j)
                    spread_alt(i,j) = 1d+0 - (q(i,j)/((mrate + (1d+0-mrate)*coupon)/(rf_rate + mrate)))
                    tax(i,j) = tax0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                    nn(i,j) = labor0_matrix(i_b_current,i_k_current,  i_y_current, i_e_current, i_crisis_current)
                    y(i,j) = EXP(z(i,j))*nn(i,j)**(alpha)

                    trans(i,j)=transfer0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                    guarantee(i,j)=guarantee0_matrix(i_b_current, i_k_current,  i_y_current, i_e_current, i_crisis_current)
                    transfer_global = trans(i,j)
                    IF (trans(i,j)>0d+0) THEN
                        tr_y_multiplier(i,j) = (output_mat_opt_transfer(i_b_current, i_k_current, i_y_current,   i_e_current) &
                        - output_mat_no_transfer(i_b_current, i_k_current, i_y_current, i_e_current))/trans(i,j)
                       
                    END IF

                    pi_firm(i,j) = (1d+0-alpha)* y(i,j)
                    !rr(i,j) = r_fun(nn(i,j)) ! requires b_initial, i_default_global, transfer_global
                    rr(i,j) = r0_matrix(i_b_current,i_k_current, i_y_current, i_e_current, i_crisis_current)
                    loan(i,j)=  loan0_matrix(i_b_current,i_k_current, i_y_current, i_e_current, i_crisis_current)
                    m_effective =  m_fun(k_initial) * ((1d+0 - bc_grid(i_crisis_current)) +&
                    bc_grid(i_crisis_current) * (1d+0 - eshock(i_e_current) * k_epsilon_frac))
                    
                    cons_bank(i,j) = trans(i,j) + loan(i,j) * rr(i,j) + &
                    b_initial*(mrate+(1d+0-mrate)*coupon) -  (b(i+1,j)-(1d+0-mrate)*b_initial)*q(i,j) +&
                    m_effective - k_grid(i_k_next)
                    c(i,j) = consumption0_matrix(i_b_current, i_k_current, i_y_current, i_e_current, i_crisis_current)
                    wage(i,j)= EXP(z(i,j))*alpha*nn(i,j)**(alpha-1d+0)/(1d+0 + gamma_firm*rr(i,j))
                    tax_repayment(i,j) = tax(i,j)

                END IF
            END IF ! CLOSES THE IF-EXCLUDED LOOP

            !yy_def=EXP(z(i,j))*labor1_matrix(i_y_current, i_e_current, i_crisis_current)**(alpha)

            IF (writeout.EQ.1) THEN
                WRITE(31,'(5F12.8,I12,6F12.8,I12,F12.8,2I12,X,3F12.8)') y(i,j),b(i,j), q(i,j), c(i,j),nn(i,j),  dd(i,j), rr(i,j),&
                tax(i,j),pi_firm(i,j), cons_bank(i,j),loan(i,j),tax_repayment(i,j),excl(i,j), trans(i,  j),&
                i_crisis_current-1, i_e_current, spread_ss(i,j), guarantee(i,j), k(i,j)			
                !WRITE (37,'(F12.8,X,I3,X,F12.8,X,F12.8)') b(i,j)/y(i,j), dd(i,j)-1, yy_def
                !WRITE (38,'(F12.8 ,X ,F12.8, X, F12.8)') nn_repayment(i,j), wage(i,j), wage_repayment(i,j)
            ENDIF
        
        END DO
    END DO

 

    IF (writeout.EQ.1) THEN
        CLOSE(31)
        CLOSE(32)
        !CLOSE(37)
        !CLOSE(38)
        OPEN (unit=9, file="graphs_simulation_spread.txt", position="rewind")
        WRITE (9, '(F15.8)') spread_ss
        CLOSE (unit=9)
    END IF

  

    sum_defaults = SUM(dd(period_start:period_num,:)-1)
    def_prob_unconditional = sum_defaults/((period_num-period_start+1)*sample_num)

    counter_def_conditional = 0
    DO j=1,sample_num  
       DO i = period_start, period_num
          IF(dd(i,j)==2 .AND. bc_crisis(i-1,j)==1) THEN
             counter_def_conditional = counter_def_conditional +1
          END IF
       END DO
    END DO

    sum_bc = SUM(bc_crisis(period_start:period_num,:))
    def_prob_conditional = counter_def_conditional/sum_bc
    def_banking_crisis = sum_bc/(sample_num*period_num)

    WRITE(*,*) 'def prob unconditional (%)', 100d+0 * def_prob_unconditional
    WRITE(*,*) 'def prob conditional on BC (%)', 100d+0 * def_prob_conditional
    WRITE(*,*) 'banking crisis probability (%)', 100d+0 * def_banking_crisis


 
    PRINT *, 'output-transfer multiplier:',  &
        SUM(tr_y_multiplier(period_start:period_num,:), tr_y_multiplier(period_start:period_num,:)>-999d+0) &
        /DBLE(MAX(1,COUNT(tr_y_multiplier(period_start:period_num,:)>-999d+0)))

    avg_tr = SUM(trans(period_start:period_num,:),                                          &
        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d+0)  &
        /DBLE(MAX(1,COUNT(trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d+0)))
    avg_b = SUM(b(period_start:period_num,:),                                               &
        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d+0)   &
        /DBLE(MAX(1,COUNT(trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d+0)))

    PRINT *, 'corr(b,tr):', SUM( (trans(period_start:period_num,:)-avg_tr)*                 &
        (b(period_start:period_num,:)-avg_b),                                               &
        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d+0 )  &
        /SQRT( SUM( (trans(period_start:period_num,:)-avg_tr)**2,                           &
        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d+0 )  &
        * SUM( (b(period_start:period_num,:)-avg_b)**2,                                     &
        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d+0 ) )

   

    avg_loan_to_y = SUM(loan(period_start:period_num,:)/y(period_start:period_num,:),   &
        excl(period_start:period_num,:).EQ.1)/COUNT(excl(period_start:period_num,:).EQ.1)
    avg_loan_to_y_def = SUM(loan(period_start:period_num,:)/y(period_start:period_num,:),   &
    excl(period_start:period_num,:).EQ.2)/COUNT(excl(period_start:period_num,:).EQ.2)       
    avg_loan_def = SUM(loan(period_start:period_num,:),   &
    excl(period_start:period_num,:).EQ.2)/COUNT(excl(period_start:period_num,:).EQ.2)    

    avg_loan= SUM(loan(period_start:period_num,:),   &
    excl(period_start:period_num,:).EQ.1)/COUNT(excl(period_start:period_num,:).EQ.1)    
    
    loan_drop = 1d+0 -  avg_loan_def/avg_loan

    avg_transfer_to_y = SUM(trans(period_start:period_num,:)/y(period_start:period_num,:),  &
        excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1)/     &
        COUNT(excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1)
    avg_log_y = SUM( LOG(y(period_start:period_num,:)),                                 &
        excl(period_start:period_num,:).EQ.1 )/COUNT(excl(period_start:period_num,:).EQ.1)
    avg_std_log_y = SQRT( SUM( (LOG(y(period_start:period_num,:))-avg_log_y)**2,        &
        excl(period_start:period_num,:).EQ.1 )/COUNT(excl(period_start:period_num,:).EQ.1) )
    avg_g_to_y = SUM( gov_spending/y(period_start:period_num,:),                        &
        excl(period_start:period_num,:).EQ.1)/COUNT(excl(period_start:period_num,:).EQ.1)
    avg_b_to_y = SUM( b(period_start+1:period_num,:)/y(period_start+1:period_num,:),    &
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    avg_b = SUM( b(period_start+1:period_num,:),                                        &
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    
    avg_k = SUM( k(period_start+1:period_num,:),                                        &
    excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    avg_k_to_y = SUM( k(period_start+1:period_num,:)/y(period_start+1:period_num,:), &
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)

    avg_k_to_assets = SUM(k(period_start+1:period_num,:)/&
    (b(period_start+1:period_num,:)+k(period_start+1:period_num,:)), &
    excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)        

    avg_M_k = SUM( M_k(period_start+1:period_num,:), &   
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    avg_exposure_1 = SUM( exposure_1(period_start+1:period_num,:), &   
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    avg_exposure_2 = SUM( exposure_2(period_start+1:period_num,:), &   
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    avg_sum_of_A = SUM( sum_of_A(period_start+1:period_num,:), &   
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    avg_frac_endog_A = SUM( Frac_endog_A(period_start+1:period_num,:), &   
        excl(period_start:period_num-1,:).EQ.1)/COUNT(excl(period_start:period_num-1,:).EQ.1)
    avg_spread = SUM( spread_ss(period_start:period_num,:),                             &
        excl(period_start:period_num,:).EQ.1 )/COUNT(excl(period_start:period_num,:).EQ.1)
    avg_std_spread = SQRT( SUM( (spread_ss(period_start:period_num,:)-avg_spread)**2,   &
        excl(period_start:period_num,:).EQ.1 )/COUNT(excl(period_start:period_num,:).EQ.1) )
    avg_spread_bc = SUM( spread_ss(period_start:period_num,:),                          &
        excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1 )     &
        /COUNT(excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1)
    avg_std_spread_bc = SQRT( SUM( (spread_ss(period_start:period_num,:)-avg_spread_bc)**2, &
        excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1 )     &
        /COUNT(excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1) )
    avg_y = SUM( y(period_start:period_num,:), excl(period_start:period_num,:).EQ.1 )/  &
        COUNT(excl(period_start:period_num,:).EQ.1)
    avg_c = SUM( c(period_start:period_num,:), excl(period_start:period_num,:).EQ.1 )/  &
        COUNT(excl(period_start:period_num,:).EQ.1)
    avg_ytm_bond = SUM( ytm_bond(period_start:period_num,:), excl(period_start:period_num,:).EQ.1 )/  &
        COUNT(excl(period_start:period_num,:).EQ.1)
    avg_ytm_free = SUM( ytm_free(period_start:period_num,:), excl(period_start:period_num,:).EQ.1 )/  &
        COUNT(excl(period_start:period_num,:).EQ.1)
    avg_spread_alt = SUM( spread_alt(period_start:period_num,:),                             &
        excl(period_start:period_num,:).EQ.1 )/COUNT(excl(period_start:period_num,:).EQ.1)
    avg_corr_spread_y = SUM( (spread_ss(period_start:period_num,:)-avg_spread)*         &
    (y(period_start:period_num,:)-avg_y), excl(period_start:period_num,:).EQ.1)     &
    /SQRT( SUM( (spread_ss(period_start:period_num,:)-avg_spread)**2, excl(period_start:period_num,:).EQ.1 ) &
    * SUM( (y(period_start:period_num,:)-avg_y)**2, excl(period_start:period_num,:).EQ.1 ) )
    avg_rr = SUM( rr(period_start:period_num,:), excl(period_start:period_num,:).EQ.1)/ &
    COUNT(excl(period_start:period_num,:).EQ.1)

    avg_rr_BC = SUM( rr(period_start:period_num,:),                                         &
    excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1)/ &
    COUNT(excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1)

    avg_rr_DEF = SUM( rr(period_start:period_num,:), excl(period_start:period_num,:).EQ.2)/ &
    COUNT(excl(period_start:period_num,:).EQ.2)

    avg_rr_BC_DEF = SUM( rr(period_start:period_num,:),                                         &
        excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.2)/ &
        COUNT(excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.2)

    PRINT *, '<< Simulated moments (conditional on no exclusion) >>'
    WRITE(*,*) 'avg y', avg_y
    WRITE(*,*) 'avg c', avg_c
    WRITE(*,*) 'avg ytm bond (percent)', avg_ytm_bond*100
    WRITE(*,*) 'avg ytm free (percent)', avg_ytm_free*100
    WRITE(*,*) 'avg spread alternative (percent)', avg_spread_alt*100
    WRITE(*,*) 'avg loan DROP (%)', 100d+0 * loan_drop
    WRITE(*,*) 'avg loan to y (%)', 100d+0 * avg_loan_to_y
    WRITE(*,*) 'avg loan to y DEF (%)', 100d+0 * avg_loan_to_y_def
    WRITE(*,*) 'avg bailout to y (%)', 100d+0 * &
        SUM(guarantee(period_start:period_num,:)/y(period_start:period_num,:),  &
        excl(period_start:period_num,:).EQ.1)/COUNT(excl(period_start:period_num,:).EQ.1)
    WRITE(*,*) 'avg bailout to y (conditional on BC, %)', 100d+0 * avg_transfer_to_y
    WRITE(*,*) 'Std Dev log(y) (%)', 100d+0 * avg_std_log_y
    WRITE(*,*) 'avg g to y (%)', 100d+0 * avg_g_to_y
    WRITE(*,*) 'avg b to y (%)', 100d+0 * avg_b_to_y
    WRITE(*,*) 'avg k to y (%)', 100d+0 * avg_k_to_y
    WRITE(*,*) 'avg k to assets (%)', 100d+0 * avg_k_to_assets
    WRITE(*,*) 'avg b to y (conditional on BC, %)', 100d+0 * &
        SUM( b(period_start+1:period_num,:)/y(period_start+1:period_num,:),             &
        excl(period_start:period_num-1,:)*bc_crisis(period_start:period_num-1,:).EQ.1)  &
        /COUNT(excl(period_start:period_num-1,:)*bc_crisis(period_start:period_num-1,:).EQ.1)
    WRITE(*,*) 'avg Exposure 1 (%)', 100d+0 * avg_exposure_1
    WRITE(*,*) 'avg Exposure 2 (%)', 100d+0 * avg_exposure_2
    WRITE(*,*) 'avg Sum of A (%)', 100d+0 * avg_sum_of_A
    WRITE(*,*) 'avg Fraction of Endogenous A (%)', 100d+0 * avg_frac_endog_A
    WRITE(*,*) 'avg spread (percent):', 100d+0 * avg_spread
    WRITE(*,*) 'std spread (percent):', 100d+0 * avg_std_spread
    WRITE(*,*) 'avg spread (conditional on BC, %):', 100d+0 * avg_spread_bc
    WRITE(*,*) 'std spread (conditional on BC, %):', 100d+0 * avg_std_spread_bc
    WRITE(*,*) 'corr(spread,y)', avg_corr_spread_y
    WRITE(*,*) 'avg loan rate (percent)', 100d+0 * avg_rr
    WRITE(*,*) 'avg loan rate (conditional on BC, %)', 100d+0 * avg_rr_BC
    WRITE(*,*) 'avg loan rate (conditional on DEF, %)', 100d+0 * avg_rr_DEF
    WRITE(*,*) 'avg loan rate (conditional on BC and DEF, %)', 100d+0 * avg_rr_BC_DEF

    i = MINVAL(MINLOC(ABS(b_grid-avg_b)))
    i_k_simu = MINVAL(MINLOC(ABS(k_grid-avg_k)))
    avg_welf = avg_welf_bk(i,i_k_simu)

    WRITE(*,*) 'welfare rel. to no-bailout (%)', 100d+0 * avg_welf

    

END SUBROUTINE simulate

!============================= UTILITIES =============================



SUBROUTINE init_random_seed()
  INTEGER ::  n
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed

  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n))

  !ivalue=139719 -- This is what I had in the JMP
  seed = 139719
  !          WRITE(*,*) seed,n

  CALL RANDOM_SEED(PUT = seed)

  DEALLOCATE(seed)
END SUBROUTINE init_random_seed

SUBROUTINE nprob (z, p, q, pdf)
  !*****************************************************************************
  !
  !! NPROB computes the cumulative density of the standard normal distribution.
  !
  !  Modified:
  !
  !    13 January 2008
  !
  !  Author:
  !
  !    Original FORTRAN77 version by AG Adams.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    AG Adams,
  !    Algorithm 39:
  !    Areas Under the Normal Curve,
  !    Computer Journal,
  !    Volume 12, Number 2, May 1969, pages 197-198.
  !
  !  Parameters:
  !
  !    Input, DOUBLE PRECISION Z, divides the real line into
  !    two semi-infinite intervals, over each of which the standard normal
  !    distribution is to be integrated.
  !
  !    Output, DOUBLE PRECISION P, Q, the integrals of the standard normal
  !    distribution over the intervals ( - Infinity, Z] and
  !    [Z, + Infinity ), respectively.
  !
  !    Output, DOUBLE PRECISION PDF, the value of the standard normal
  !    distribution at Z.
  !
  implicit none
  DOUBLE PRECISION, PARAMETER :: a0 = 0.5D+00
  DOUBLE PRECISION, PARAMETER :: a1 = 0.398942280444D+00
  DOUBLE PRECISION, PARAMETER :: a2 = 0.399903438504D+00
  DOUBLE PRECISION, PARAMETER :: a3 = 5.75885480458D+00
  DOUBLE PRECISION, PARAMETER :: a4 = 29.8213557808D+00
  DOUBLE PRECISION, PARAMETER :: a5 = 2.62433121679D+00
  DOUBLE PRECISION, PARAMETER :: a6 = 48.6959930692D+00
  DOUBLE PRECISION, PARAMETER :: a7 = 5.92885724438D+00
  DOUBLE PRECISION, PARAMETER :: b0 = 0.398942280385D+00
  DOUBLE PRECISION, PARAMETER :: b1 = 0.000000038052D+00
  DOUBLE PRECISION, PARAMETER :: b2 = 1.00000615302D+00
  DOUBLE PRECISION, PARAMETER :: b3 = 0.000398064794D+00
  DOUBLE PRECISION, PARAMETER :: b4 = 1.98615381364D+00
  DOUBLE PRECISION, PARAMETER :: b5 = 0.151679116635D+00
  DOUBLE PRECISION, PARAMETER :: b6 = 5.29330324926D+00
  DOUBLE PRECISION, PARAMETER :: b7 = 4.8385912808D+00
  DOUBLE PRECISION, PARAMETER :: b8 = 15.1508972451D+00
  DOUBLE PRECISION, PARAMETER :: b9 = 0.742380924027D+00
  DOUBLE PRECISION, PARAMETER :: b10 = 30.789933034D+00
  DOUBLE PRECISION, PARAMETER :: b11 = 3.99019417011D+00
  DOUBLE PRECISION :: p, pdf, q, y, z, zabs

  zabs = abs (z)
  !
  !  |Z| between 0 and 1.28
  !
  IF ( abs ( z ) <= 1.28D+00 ) THEN

     y = a0 * z * z
     pdf = exp ( - y ) * b0

     q = a0 - zabs * ( a1 - a2 * y/( y + a3 - a4/( y + a5 + a6/( y + a7 ))))
     !
     !  |Z| between 1.28 and 12.7
     !
  ELSE IF ( abs ( z ) <= 12.7D+00 ) THEN

     y = a0 * z * z
     pdf = exp ( - y ) * b0

     q = pdf &
          / ( zabs - b1 + b2 &
          / ( zabs + b3 + b4 &
          / ( zabs - b5 + b6 &
          / ( zabs + b7 - b8 &
          / ( zabs + b9 + b10 &
          / ( zabs + b11 ))))))
     !
     !  Z far out in tail.
     !
  ELSE

     q = 0.0D+00
     pdf = 0.0D+00

  END IF

  IF ( z < 0.0D+00 ) THEN
     p = q
     q = 1.0D+00 - p
  ELSE
     p = 1.0D+00 - q
  END IF

  return
    END SUBROUTINE nprob

! ----------------------------------------------------------------------
!  SR: hpfilt
!  Kalman smoothing routine for HP filter written by E Prescott.
!   y=data series, d=deviations from trend, t=trend, n=no. obs,
!   s=smoothing parameter (eg, 1600 for std HP).
!   Array v is scratch area and must have dimension at least 3n.
! ----------------------------------------------------------------------
SUBROUTINE hpfilt(y,t,n,s)
    INTEGER, INTENT(IN) :: n ! number of observations
    DOUBLE PRECISION, INTENT(IN) ::  s ! smoothing parameter
    DOUBLE PRECISION, DIMENSION(n), INTENT(IN) ::  y ! data series
    DOUBLE PRECISION, DIMENSION(n), INTENT(OUT) ::  t ! trend series
    DOUBLE PRECISION, DIMENSION(n) ::  d ! deviation series
    DOUBLE PRECISION, DIMENSION(n,3) ::  v
    DOUBLE PRECISION ::  m1,m2,v11,v12,v22,x,z,b11,b12,b22,det,e1,e2
    INTEGER :: i,i1,ib

    !
    ! compute sequences of covariance matrix for f[x(t),x(t-1) | y(<t)]
    !

    v11 = 1d+0
    v22 = 1d+0
    v12 = 0d+0
    DO i=3,n
        x = v11
        z = v12
        v11 = 1d+0/s + 4d+0*(x-z) + v22
        v12 = 2d+0*x - z
        v22 = x
        det = v11*v22-v12*v12
        v(i,1) = v22/det
        v(i,3) = v11/det
        v(i,2) = -v12/det
        x = v11 + 1d+0
        z = v11
        v11 = v11 - v11*v11/x
        v22 = v22 - v12*v12/x
        v12 = v12 - z*v12/x
    END DO
    
    !
    ! this is the forward pass
    !
    m1 = y(2)
    m2 = y(1)
    DO i=3,n
        x = m1
        m1 = 2d+0*m1 - m2
        m2 = x
        t(i-1) = v(i,1)*m1 + v(i,2)*m2
        d(i-1) = v(i,2)*m1 + v(i,3)*m2
        det = v(i,1)*v(i,3) - v(i,2)*v(i,2)
        v11 = v(i,3)/det
        v12 = -v(i,2)/det
        z = (y(i) - m1)/(v11 + 1d+0)
        m1 = m1 + v11*z
        m2 = m2 + v12*z
    END DO
        
    t(n) = m1
    t(n-1) = m2
    
    !
    ! this is the backward pass
    !
    m1 = y(n-1)
    m2 = y(n)
    DO i=n-2,1,-1
        i1 = i+1
        ib = n - i + 1
        x = m1
        m1 = 2d+0*m1 - m2
        m2 = x
        !
        ! combine info for y(.lt.i) with info for y(.ge.i)
        !
        IF(i.GT.2) THEN
            e1 = v(ib,3)*m2 + v(ib,2)*m1 + t(i)
            e2 = v(ib,2)*m2 + v(ib,1)*m1 + d(i)
            b11 = v(ib,3) + v(i1,1)
            b12 = v(ib,2) + v(i1,2)
            b22 = v(ib,1) + v(i1,3)
            det = b11*b22 - b12*b12
            t(i) = (-b12*e1 + b11*e2)/det
        ENDIF
        !
        ! end of combining
        !
        
        det = v(ib,1)*v(ib,3) - v(ib,2)*v(ib,2)
        v11 = v(ib,3)/det
        v12 = -v(ib,2)/det
        z = (y(i) - m1)/(v11 + 1d+0)
        m1 = m1 + v11*z
        m2 = m2 + v12*z
    END DO
    
    t(1) = m1
    t(2) = m2

    d = y - t
    
END SUBROUTINE
!***********************************************************************
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Author: Wilmer Henao    wi-henao@uniandes.edu.co
!   Department of Mathematics
!   Universidad de los Andes
!   Colombia
!
!   Hodrick-Prescott filter extracts the trend of a time series, the output
!   is not a formula but a new filtered time series.  This trend can be
!   adjusted with parameter w; values for w lie usually in the interval
!   [100,20000], and it is up to you to use the one you like, As w approaches infty, 
!   H-P will approach a line.  If the series doesn't have a trend p.e.White Noise, 
!   doing H-P is meaningles
!
!   [s] = hpfilter(y,w)
!   w = Smoothing parameter (Economists advice: "Use w = 1600 for quarterly data")
!   y = Original series
!   s = Filtered series
!   This program can work with several series at a time, as long as the
!   number of series you are working with doesn't exceed the number of
!   elements in the series + it uses sparse matrices which improves speed
!   and performance in the longest series
!   
!   [s] = hpfilter(y,w,'makeplot')
!   'makeplot' in the input, plots the graphics of the original series
!   against the filtered series, if more than one series is being
!   considered the program will plot all of them in different axes
!
!   [s,desvabs] = hpfilter(y,w)
!   Gives you a mesure of the standardized differences in absolute values
!   between the original and the filtered series.  A big desvabs means
!   that the series implies a large relative volatility.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!FUNCTION hpfilter(y,m,n,w)
!    IMPLICIT NONE
!    DOUBLE PRECISION, INTENT(IN) :: w
!    INTEGER, INTENT(IN) :: m,n
!    DOUBLE PRECISION, DIMENSION(m,n), INTENT(IN) :: y
!    INTEGER :: i
!    
!    IF (m<n) ERROR STOP 'Number of series cannot exceed number of elements!'
!    
!    ! d = repmat([w -4*w ((6*w+1)/2)], m, 1);
!    DO i=1,m
!        d(i,:) = (/ w, -4d+0*w, (6d+0*w+1d+0)/2 /)
!    END DO
!
!    !d(1,2) = -2*w;      d(m-1,2) = -2*w;
!    !d(1,3) = (1+w)/2;   d(m,3) = (1+w)/2;
!    !d(2,3) = (5*w+1)/2; d(m-1,3) = (5*w+1)/2;
!    d(1,2) = -2d+0*w
!    d(m-1,2) = -2d+0*w
!    d(1,3) = (1d+0+w)/2
!    d(m,3) = (1d+0+w)/2
!    d(2,3) = (5d+0*w+1d+0)/2
!    d(m-1,3) = (5d+0*w+1d+0)/2
!    
!    !B = spdiags(d, -2:0, m, m);    %I use a sparse version of B, because when m is large, B will have many zeros     
!    !B = B+B';
!    !s = B\y;
!
!END FUNCTION hpfilter
    

function zbrent(func,x1,x2,tol)
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN) :: x1,x2,tol
   DOUBLE PRECISION :: zbrent
   INTERFACE
      FUNCTION func(x)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: x
      DOUBLE PRECISION :: func
      END FUNCTION func
   END INTERFACE
   INTEGER, PARAMETER :: ITMAX=500
   DOUBLE PRECISION, PARAMETER :: EPS=epsilon(x1)
   INTEGER :: iter
   DOUBLE PRECISION :: a,b,c,d,e,fa,fb,fc,p,q,rr,s,tol1,xm
   a=x1
   b=x2
   fa=func(a)
   fb=func(b)
   IF ((fa > 0.0 .and. fb > 0.0) .or. (fa < 0.0 .and. fb < 0.0)) THEN
      WRITE(*,*) 'root must be bracketed for zbrent. Hit ENTER to continue'
      READ(*,*)
      ! pause
      ! WRITE(6,*) 'root must be bracketed for zbrent'
   END IF
   c=b
   fc=fb
   DO iter=1,ITMAX
      IF ((fb > 0.0 .and. fc > 0.0) .or. (fb < 0.0 .and. fc < 0.0)) THEN
         c=a
         fc=fa
         d=b-a
         e=d
      END IF
      IF (abs(fc) < abs(fb)) THEN
         a=b
         b=c
         c=a
         fa=fb
         fb=fc
         fc=fa
      END IF
      tol1=2.0d+0*EPS*abs(b)+0.5d+0*tol
      xm=0.5d+0*(c-b)
      IF (abs(xm) <= tol1 .or. fb == 0.0) THEN
         zbrent=b
         RETURN
      END IF
      IF (abs(e) >= tol1 .and. abs(fa) > abs(fb)) THEN
         s=fb/fa
         IF (a == c) THEN
            p=2.0d+0*xm*s
            q=1.0d+0-s
         ELSE
            q=fa/fc
            rr=fb/fc
            p=s*(2.0d+0*xm*q*(q-rr)-(b-a)*(rr-1.0d+0))
            q=(q-1.0d+0)*(rr-1.0d+0)*(s-1.0d+0)
         END IF
         IF (p > 0.0) q=-q
         p=abs(p)
         IF (2.0d+0*p  <  min(3.0d+0*xm*q-abs(tol1*q),abs(e*q))) THEN
            e=d
            d=p/q
         ELSE
            d=xm
            e=d
         END IF
      ELSE
         d=xm
         e=d
      END IF
      a=b
      fa=fb
      b=b+merge(d,sign(tol1,xm), abs(d) > tol1 )
      fb=func(b)
    END DO

    WRITE(*,*)'zbrent: exceeded maximum iterations. Hit ENTER to continue'
    READ(*,*)

    ! PAUSE
  !   WRITE(6, *) 'zbrent: exceeded maximum iterations'

  zbrent=b
END FUNCTION zbrent

SUBROUTINE pareto_cdf(lb, ub, a, x,cdf)
!cdf function for bounded Pareto distribution
!lb = lower bound
!ub = upper bound
!a = shape PARAMETER
DOUBLE PRECISION, INTENT (IN)  :: lb, ub, a, x
DOUBLE PRECISION, INTENT (OUT) :: cdf

    cdf = (1.0d+0 -((lb/x)**a))/(1.0d+0-((lb/ub)**a))

END SUBROUTINE pareto_cdf

SUBROUTINE mom_pareto(lb,ub,a,k, mom)
! moment function for bounded Pareto distibution
!lb = lower bound
!ub = upper bound
!a = shape PARAMETER
!k = moment
DOUBLE PRECISION, INTENT (IN)  :: lb, ub, a,k
DOUBLE PRECISION, INTENT (OUT) :: mom

mom = (a*(lb**k)/(a-k))*(1.0d+0 -(lb/ub)**(a-k))/(1.0d+0 -(lb/ub)**a)

END SUBROUTINE mom_pareto
