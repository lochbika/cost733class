!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine newseed()
  ! initiates the random number generator
  implicit none
  integer :: nseed,i,j,iseed
  integer, allocatable :: seeds(:)
  integer dates(8)
  ! Lili Ju's set random seed
  iseed=0
  call random_seed
  call random_seed (size=nseed)
  allocate(seeds(nseed))
  call date_and_time(values=dates)
  do i = 1, nseed
     seeds(i) = 0
     do j = 1, 8
        seeds(i)=seeds(i)+(j+i)*dates(j)+iseed*100
        seeds(i)=ishftc(seeds(i),4*(j-1))
     enddo
  enddo
  call random_seed (put=seeds)
  deallocate(seeds)
end subroutine newseed

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine newseedi(iseed)
  ! initiates the random number generator
  implicit none
  integer :: nseed,i,j,iseed
  integer, allocatable :: seeds(:)
  integer dates(8)
  ! Lili Ju's set random seed
  !iseed=0
  call random_seed
  call random_seed (size=nseed)
  allocate(seeds(nseed))
  call date_and_time(values=dates)
  do i = 1, nseed
     seeds(i) = 0
     do j = 1, 8
        seeds(i)=seeds(i)+(j+i)*dates(j)+iseed*100
        seeds(i)=ishftc(seeds(i),4*(j-1))
     enddo
  enddo
  call random_seed (put=seeds)
  deallocate(seeds)
end subroutine newseedi
