!
!  Copyright 2017 ARTED developers
!
!  Licensed under the Apache License, Version 2.0 (the "License");
!  you may not use this file except in compliance with the License.
!  You may obtain a copy of the License at
!
!      http://www.apache.org/licenses/LICENSE-2.0
!
!  Unless required by applicable law or agreed to in writing, software
!  distributed under the License is distributed on an "AS IS" BASIS,
!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!  See the License for the specific language governing permissions and
!  limitations under the License.
!
subroutine core_matrix_inverse(amat,nn)
  implicit none
  integer,intent(in)    :: nn
  real(8),intent(inout) :: amat(nn,nn)
  integer :: i,j
  real(8) :: emat(nn,nn)
  integer :: nrhs,lda,ldb,info
  integer :: ipiv(nn)

  nrhs=nn
  lda=nn
  ldb=nn

  do j=1,nn
    do i=1,nn
      emat(i,j)=0.d0
    end do
  end do

  do i=1,nn
    emat(i,i)=1.d0
  end do

  call dgesv(nn,nrhs,amat,lda,ipiv,emat,ldb,info)

  do j=1,nn
    do i=1,nn
      amat(i,j)=emat(i,j)
    end do
  end do

end subroutine core_matrix_inverse
