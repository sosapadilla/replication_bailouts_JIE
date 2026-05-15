module report_tables
  implicit none
  private
  public :: write_replication_outputs, print_run_summary

contains

  subroutine write_replication_outputs(output_dir, model_label, &
       def_prob_unconditional, def_prob_conditional, def_banking_crisis, &
       avg_g_to_y, avg_transfer_to_y, avg_spread, avg_std_spread, avg_corr_spread_y, &
       avg_b_to_y, bailout_debt_corr, tr_multiplier, banking_sector_loss, &
       avg_spread_bc, avg_std_spread_bc, avg_b_to_y_bc, avg_bailout_to_y_uncond, &
       avg_rr, avg_rr_bc)

    implicit none

    character(len=*), intent(in) :: output_dir, model_label
    double precision, intent(in) :: def_prob_unconditional, def_prob_conditional, def_banking_crisis
    double precision, intent(in) :: avg_g_to_y, avg_transfer_to_y, avg_spread, avg_std_spread
    double precision, intent(in) :: avg_corr_spread_y, avg_b_to_y, bailout_debt_corr
    double precision, intent(in) :: tr_multiplier, banking_sector_loss
    double precision, intent(in) :: avg_spread_bc, avg_std_spread_bc, avg_b_to_y_bc
    double precision, intent(in) :: avg_bailout_to_y_uncond, avg_rr, avg_rr_bc

    integer :: u2, u3, u4, us
    character(len=512) :: f2, f3, f4, fs

    f2 = trim(output_dir)//'/table2_model_vs_data.csv'
    f3 = trim(output_dir)//'/table3_regime_comparison.csv'
    f4 = trim(output_dir)//'/table4_model_comparison.csv'
    fs = trim(output_dir)//'/run_summary.txt'

    call open_replace(u2, f2)
    write(u2,'(A)') 'table,statistic,model_value,data_value,notes'
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Default frequency (%)',            100d0*def_prob_unconditional, 'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Banking crisis frequency (%)',     100d0*def_banking_crisis,     'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Government spending/GDP (%)',      100d0*avg_g_to_y,             'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Bailouts/GDP | banking crisis (%)',100d0*avg_transfer_to_y,      'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Sovereign spread mean (%)',        100d0*avg_spread,             'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Sovereign spread sd (%)',          100d0*avg_std_spread,         'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'corr(spread,y)',                   avg_corr_spread_y,            'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Debt/GDP (%)',                     100d0*avg_b_to_y,             'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'corr(bailouts,debt)',              bailout_debt_corr,            'Fill data_value from paper/data source')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Bailout-output multiplier',        tr_multiplier,                'Model-implied statistic')
    call write_csv_row_with_blank_local(u2, 'Table 2', 'Banking sector loss (%)',          100d0*banking_sector_loss,    'Model-implied statistic')
    close(u2)

    call open_replace(u3, f3)
    write(u3,'(A)') 'table,panel,statistic,value'
    call write_csv_row_local(u3, 'Table 3', 'Unconditional',  'Default frequency (%)',       100d0*def_prob_unconditional)
    call write_csv_row_local(u3, 'Table 3', 'Unconditional',  'Sovereign spread mean (%)',   100d0*avg_spread)
    call write_csv_row_local(u3, 'Table 3', 'Unconditional',  'Sovereign spread sd (%)',     100d0*avg_std_spread)
    call write_csv_row_local(u3, 'Table 3', 'Unconditional',  'Debt/GDP (%)',                100d0*avg_b_to_y)
    call write_csv_row_local(u3, 'Table 3', 'Unconditional',  'Bailout/GDP (%)',             100d0*avg_bailout_to_y_uncond)

    call write_csv_row_local(u3, 'Table 3', 'Banking crisis', 'Default frequency (%)',       100d0*def_prob_conditional)
    call write_csv_row_local(u3, 'Table 3', 'Banking crisis', 'Sovereign spread mean (%)',   100d0*avg_spread_bc)
    call write_csv_row_local(u3, 'Table 3', 'Banking crisis', 'Sovereign spread sd (%)',     100d0*avg_std_spread_bc)
    call write_csv_row_local(u3, 'Table 3', 'Banking crisis', 'Debt/GDP (%)',                100d0*avg_b_to_y_bc)
    call write_csv_row_local(u3, 'Table 3', 'Banking crisis', 'Bailout/GDP (%)',             100d0*avg_transfer_to_y)
    close(u3)

    call open_replace(u4, f4)
    write(u4,'(A)') 'table,model,panel,statistic,value'
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Unconditional', 'Default frequency (%)',     100d0*def_prob_unconditional)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Unconditional', 'Sovereign spread mean (%)', 100d0*avg_spread)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Unconditional', 'Sovereign spread sd (%)',   100d0*avg_std_spread)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Unconditional', 'Debt/GDP (%)',              100d0*avg_b_to_y)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Unconditional', 'Mean lending rate (%)',     100d0*avg_rr)

    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Banking crisis', 'Default frequency (%)',     100d0*def_prob_conditional)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Banking crisis', 'Sovereign spread mean (%)', 100d0*avg_spread_bc)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Banking crisis', 'Sovereign spread sd (%)',   100d0*avg_std_spread_bc)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Banking crisis', 'Debt/GDP (%)',              100d0*avg_b_to_y_bc)
    call write_csv_row_model_local(u4, 'Table 4', trim(model_label), 'Banking crisis', 'Mean lending rate (%)',     100d0*avg_rr_bc)
    close(u4)

    call open_replace(us, fs)
    write(us,'(A)') 'Replication run summary'
    write(us,'(A)') '======================='
    write(us,'(A)') 'Model label: '//trim(model_label)
    write(us,'(A)') 'Files written:'
    write(us,'(A)') '  - table2_model_vs_data.csv'
    write(us,'(A)') '  - table3_regime_comparison.csv'
    write(us,'(A)') '  - table4_model_comparison.csv'
    write(us,'(A)') ''
    write(us,'(A,F12.6)') 'Default frequency (%): ', 100d0*def_prob_unconditional
    write(us,'(A,F12.6)') 'Mean spread (%):       ', 100d0*avg_spread
    write(us,'(A,F12.6)') 'Debt/GDP (%):          ', 100d0*avg_b_to_y
    close(us)

  end subroutine write_replication_outputs


  subroutine print_run_summary(model_label, def_prob_unconditional, avg_spread, avg_b_to_y)
    implicit none
    character(len=*), intent(in) :: model_label
    double precision, intent(in) :: def_prob_unconditional, avg_spread, avg_b_to_y

    write(*,'(A)') ''
    write(*,'(A)') '----------------------------------------'
    write(*,'(A)') 'Replication summary'
    write(*,'(A)') '----------------------------------------'
    write(*,'(A,A)') 'Model: ', trim(model_label)
    write(*,'(A,F10.4)') 'Default frequency (%): ', 100d0*def_prob_unconditional
    write(*,'(A,F10.4)') 'Mean spread (%):       ', 100d0*avg_spread
    write(*,'(A,F10.4)') 'Debt/GDP (%):          ', 100d0*avg_b_to_y
    write(*,'(A)') 'Detailed tables were written to CSV files.'
    write(*,'(A)') ''
  end subroutine print_run_summary


  subroutine open_replace(unit_no, file_name)
    implicit none
    integer, intent(out) :: unit_no
    character(len=*), intent(in) :: file_name
    logical :: is_open
    integer :: k

    do k = 20, 200
      inquire(unit=k, opened=is_open)
      if (.not. is_open) then
        unit_no = k
        open(unit=unit_no, file=trim(file_name), status='replace', action='write', form='formatted')
        return
      end if
    end do

    error stop 'open_replace: could not find a free unit number.'
  end subroutine open_replace


subroutine write_csv_row_local(unit_no, table_name, panel_name, stat_name, value)
    implicit none
    integer, intent(in) :: unit_no
    character(len=*), intent(in) :: table_name, panel_name, stat_name
    double precision, intent(in) :: value

    write(unit_no,'(A,",",A,",",A,",",F18.8)') &
         trim(table_name), trim(panel_name), trim(stat_name), value
end subroutine write_csv_row_local

subroutine write_csv_row_model_local(unit_no, table_name, model_name, panel_name, stat_name, value)
    implicit none
    integer, intent(in) :: unit_no
    character(len=*), intent(in) :: table_name, model_name, panel_name, stat_name
    double precision, intent(in) :: value

    write(unit_no,'(A,",",A,",",A,",",A,",",F18.8)') &
         trim(table_name), trim(model_name), trim(panel_name), trim(stat_name), value
end subroutine write_csv_row_model_local


subroutine write_csv_row_with_blank_local(unit_no, table_name, stat_name, model_value, notes)
    implicit none
    integer, intent(in) :: unit_no
    character(len=*), intent(in) :: table_name, stat_name, notes
    double precision, intent(in) :: model_value

    write(unit_no,'(A,",",A,",",F18.8,",,",A)') &
         trim(table_name), trim(stat_name), model_value, trim(notes)
end subroutine write_csv_row_with_blank_local


  function real_to_text(x) result(txt)
    implicit none
    double precision, intent(in) :: x
    character(len=64) :: txt
    write(txt,'(F18.8)') x
    txt = adjustl(txt)
  end function real_to_text


  function csv_escape(s) result(out)
    implicit none
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: out
    character(len=:), allocatable :: tmp
    integer :: i

    tmp = ''
    do i = 1, len_trim(s)
      if (s(i:i) == '"') then
        tmp = tmp // '""'
      else
        tmp = tmp // s(i:i)
      end if
    end do
    out = '"' // tmp // '"'
  end function csv_escape

end module report_tables


! ------------------------------------------------------------------
! Integration notes:
!
! 1. Compile this file together with your main program.
!
! 2. In the main program, add:
!       use report_tables
!
! 3. In simulate, compute these two scalars before the final print block:
!
!    avg_b_to_y_bc = SUM( b(period_start+1:period_num,:)/y(period_start+1:period_num,:), &
!        excl(period_start:period_num-1,:)*bc_crisis(period_start:period_num-1,:).EQ.1) &
!        / COUNT(excl(period_start:period_num-1,:)*bc_crisis(period_start:period_num-1,:).EQ.1)
!
!    avg_bailout_to_y_uncond = SUM(guarantee(period_start:period_num,:)/y(period_start:period_num,:), &
!        excl(period_start:period_num,:).EQ.1) / COUNT(excl(period_start:period_num,:).EQ.1)
!
!    avg_rr_bc = SUM(rr(period_start:period_num,:), &
!        excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1) / &
!        COUNT(excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1)
!
!    bailout_debt_corr = SUM( (trans(period_start:period_num,:)-avg_tr) * &
!        (b(period_start:period_num,:)-avg_b), &
!        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d0 ) / &
!        SQRT( SUM( (trans(period_start:period_num,:)-avg_tr)**2, &
!        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d0 ) * &
!        SUM( (b(period_start:period_num,:)-avg_b)**2, &
!        trans(period_start:period_num,:)*(2-excl(period_start:period_num,:))>0d0 ) )
!
!    banking_sector_loss = SUM(exposure_rate(period_start:period_num,:), &
!        excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1) / &
!        COUNT(excl(period_start:period_num,:)*bc_crisis(period_start:period_num,:).EQ.1)
!
! 4. Then call:
!
!    call print_run_summary('baseline model', def_prob_unconditional, avg_spread, avg_b_to_y)
!
!    call write_replication_outputs(trim(path), 'baseline model', &
!         def_prob_unconditional, def_prob_conditional, def_banking_crisis, &
!         avg_g_to_y, avg_transfer_to_y, avg_spread, avg_std_spread, avg_corr_spread_y, &
!         avg_b_to_y, bailout_debt_corr, tr_multiplier, banking_sector_loss, &
!         avg_spread_bc, avg_std_spread_bc, avg_b_to_y_bc, avg_bailout_to_y_uncond, &
!         avg_rr, avg_rr_bc)
!
! 5. After that, you can delete the long Table 2 / 3 / 4 WRITE(*,*) blocks
!    from simulate and keep only the short console summary.
! ------------------------------------------------------------------
