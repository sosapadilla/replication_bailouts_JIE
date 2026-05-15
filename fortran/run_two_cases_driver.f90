program run_two_cases_driver
  implicit none
  integer :: status

  print *, '=== Two-case Fortran driver ==='
  print *, 'This driver expects bailouts_optimal.f90 in the current folder.'

  call run_cmd('rm -rf nb_files')
  call run_cmd('mkdir -p nb_files')

  ! ---------------------------------------------------------------
  ! 1) NO-BAILOUT RUN
  !    Note: transfer_num=0 is not safe in this code because many arrays
  !    are dimensioned as 1:transfer_num and MAXLOC is called over
  !    transfer_num objects. transfer_num=1 with T_max_frac=0 is the
  !    numerically equivalent no-transfer/no-bailout case.
  ! ---------------------------------------------------------------
  print *, '--- Creating no-bailout source ---'
  call run_cmd('cp bailouts_optimal.f90 bailouts_optimal_nobailout_auto.f90')
  call patch_file('bailouts_optimal_nobailout_auto.f90', 'NOB')

  print *, '--- Compiling no-bailout source ---'
  call run_cmd('rm -f PARAM.mod param.mod bailouts_optimal_nobailout_auto.out')
  call run_cmd('gfortran -O3 bailouts_optimal_nobailout_auto.f90 -o bailouts_optimal_nobailout_auto.out')

  print *, '--- Running no-bailout case ---'
  call run_cmd('./bailouts_optimal_nobailout_auto.out')

  print *, '--- Saving no-bailout outputs to nb_files/ ---'
  call run_cmd('cp -f results.out nb_files/results_nobailout.out 2>/dev/null || true')
  call run_cmd('cp -f graphs_* nb_files/ 2>/dev/null || true')
  call run_cmd('cp -f output_* nb_files/ 2>/dev/null || true')
  call run_cmd('cp -f objective_* nb_files/ 2>/dev/null || true')
  call run_cmd('cp -f tax_vector.txt bp_choice.txt nb_files/ 2>/dev/null || true')

  ! ---------------------------------------------------------------
  ! 2) BAILOUT RUN
  ! ---------------------------------------------------------------
  print *, '--- Creating bailout source ---'
  call run_cmd('cp bailouts_optimal.f90 bailouts_optimal_bailout_auto.f90')
  call patch_file('bailouts_optimal_bailout_auto.f90', 'BAIL')

  print *, '--- Compiling bailout source ---'
  call run_cmd('rm -f PARAM.mod param.mod bailouts_optimal_bailout_auto.out')
  call run_cmd('gfortran -O3 bailouts_optimal_bailout_auto.f90 -o bailouts_optimal_bailout_auto.out')

  print *, '--- Running bailout case ---'
  call run_cmd('./bailouts_optimal_bailout_auto.out')

  print *, '=== Done ==='
  print *, 'No-bailout outputs are in nb_files/.'
  print *, 'Bailout outputs are in the current folder.'

contains

  subroutine run_cmd(cmd)
    character(len=*), intent(in) :: cmd
    integer :: status
    print *, trim(cmd)
    call execute_command_line(trim(cmd), exitstat=status)
    if (status /= 0) then
       print *, 'Command failed with status ', status
       print *, trim(cmd)
       stop 1
    end if
  end subroutine run_cmd

  subroutine patch_file(fname, mode)
    character(len=*), intent(in) :: fname, mode
    character(len=4096) :: cmd

    if (mode == 'NOB') then
       ! runwelf = 0
       write(cmd,'(A)') 'perl -0pi -e ''s/INTEGER ::\s+writeout\s*=\s*1,\s*simulonly\s*=\s*0,\s*maxiter\s*=\s*([0-9]+),\s*runwelf\s*=\s*[01]/INTEGER :: \twriteout = 1, simulonly = 0, maxiter = $1, runwelf = 0/s'' '//trim(fname)
       call run_cmd(cmd)

       ! transfer_num = 1 rather than 0 to avoid zero-size array and MAXLOC failures.
       write(cmd,'(A)') 'perl -0pi -e ''s/transfer_num\s*=\s*[0-9]+/transfer_num = 1/s'' '//trim(fname)
       call run_cmd(cmd)

       ! T_max_frac = 0.0d+0
       write(cmd,'(A)') 'perl -0pi -e ''s/T_max_frac\s*=\s*[-+0-9.]+d[+-]?0/T_max_frac = 0.0d+0/s'' '//trim(fname)
       call run_cmd(cmd)

    else
       ! runwelf = 1
       write(cmd,'(A)') 'perl -0pi -e ''s/INTEGER ::\s+writeout\s*=\s*1,\s*simulonly\s*=\s*0,\s*maxiter\s*=\s*([0-9]+),\s*runwelf\s*=\s*[01]/INTEGER :: \twriteout = 1, simulonly = 0, maxiter = $1, runwelf = 1/s'' '//trim(fname)
       call run_cmd(cmd)

       ! transfer_num = 50
       write(cmd,'(A)') 'perl -0pi -e ''s/transfer_num\s*=\s*[0-9]+/transfer_num = 50/s'' '//trim(fname)
       call run_cmd(cmd)

       ! T_max_frac = 1.0d+0
       write(cmd,'(A)') 'perl -0pi -e ''s/T_max_frac\s*=\s*[-+0-9.]+d[+-]?0/T_max_frac = 1.0d+0/s'' '//trim(fname)
       call run_cmd(cmd)
    end if
  end subroutine patch_file

end program run_two_cases_driver
