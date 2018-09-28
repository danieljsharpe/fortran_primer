program linkedlist

type node
real :: item
type(node), pointer :: next
end type node

! This derived data type contains a single object item (the data in the list) and a pointer "next" to another instance of node. Note the recursion-like property in the
! declaration allowing the pointer to reference its own data type

! IMPLEMENTING A LINKED LIST
type(node), pointer :: firstnode
type(node), pointer :: anode ! used to dynamically change its target during runtime

!anode => firstnode
!anode%item ! now anode can access data of the first element
!anode => anode%next ! used to access the next element
!anode%item ! now anode can access data of the second element
! and so on....

! For real this time:
! create linked list
allocate(firstnode, stat=IERR4)
firstnode%item = 1.0
anode => firstnode
do i =2,10
    allocate(anode%next)
    anode%next%item = float(i)
    anode => anode%next
end do
nullify(anode%next) ! for the last elem, the "next" pointer has not been linked with an elem. To make sure that it does not point to a random part in the memory, we nullify it

!read the data in the linked list. Note we do not need to know how large the linked list is! Since we ensured that the last "next" pointer is nullified, we will receive FALSE
!for the associated enquiry and the loop will end. We can add or subtract elements of the list without changing the definition of the loop. THis allows us to dynamically
!create elements or delete them
anode => firstnode
do while(associated(anode))
    write(*,*) anode%item
    anode => anode%next
end do

end program linkedlist
