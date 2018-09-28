! A POINTER is a type variable which may reference the data stored by other variables (the TARGET) or areas of dynamically allocated memory
! They serve as a flexible alternative to allocatable arrays and are tools to create and manipulate dynamic data structures (such as linked lists)
! The use of pointers allows programmers to implement powerful algorithms and tailor the storage requirements exactly to the size of the problem in hand

! Pointers are best thought of as variables which are dynamically associated with (or aliased to) some target data. Pointers are said to 'point to' their
! targets and valid targets include:
! - variables of the same data type as the pointers and explicitly declared with the TARGET attribute
! - other pointers of the same data type
! - dynamic memory allocated to the pointer
! Pointers may take advantage of dynamic storage but do not require the ALLOCATABLE attribute. The ability to allocate and deallocate storage is an
! inherent property of pointer variables

! A pointer must have the same data point and rank as its target.  For array pointers the declaration statement must specify the rank but not the shape
! (i.e. bounds) of the array. In this respect pointers are similar to allocatable arrays

program main

implicit none

integer :: i, j
integer, target, dimension(3,3) :: h

! Some example pointers and one or more variables which may be targets. Note additional attributes can be given after the POINTER or TARGET attribute is declared.
! However, both targets and pointers are incompatible with the EXTERNAL, INTRINSIC and PARAMETER attributes. The POINTER attribute is also incompatible with
! the ALLOCATABLE and INTENT attributes.

real, pointer :: pt1
real, target :: a, b, c

integer, dimension(:), pointer :: pt2
integer, target :: d(3), e(6), f(9)

integer, pointer :: pt3(:,:)
integer, target, allocatable :: g(:,:)
integer, pointer :: pt4(:,:)

! There are two operators which may act on pointers
! 1. the pointer assignment operator (pointer => target), used to associate a pointer variable with any valid target. The pointer may now be used as an alias
! to the data stored by the target. The pointer assignment operator also allocates storage required by the pointer

allocate(g (3, 3))

pt3 => g ! pt3 points to g (target g now associated with pointer pt3)

! 2. the assignment operator (pointer = x) is used to change the value of a pointer's target, just like changing the value of a variable, except now the
! pointer acts as an alias to another variable

pt3 = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /), order=(/2,1/)) ! the value of g (target of pt3) changes. i.e. new value assigned to pt3's target
                                                                          ! (not to pt3 itself)
write(*,*) ( (g(i,j) , j = 1,3), i=1,3 )
h = pt3 ! h = g; i.e. value of h changed to be the same as pt3's target. Valid only if pt3 is associated with a target
write(*,*) ( (h(i,j), j = 1,3), i = 1,3)
pt3 => h ! pt3 re-assigned; now points to h
h = reshape((/ 2, 3, 4, 5, 6, 7, 8, 9, 10 /), (/ 3, 3 /), order=(/2,1/)) ! 
write(*,*) ( (pt3(i,j), j = 1,3), i = 1,3) ! Note: g != h
pt4 => pt3 ! pt4 points to h (the target of pt3)
write(*,*) ( (pt4(i,j), j = 1,3), i =1,3)

! Note in the above that you can point to a pointer. Note that this is valid ONLY when the pointer being 'pointed to' has an associated target, since this
! operation is actually the first pointer pointing to the second's target; it is wrong to think of 'chains of pointers', one pointing to another.
! Note that pointing to constants or to arithmetic expressions is illegal; neither are valid targets
! i.e. pt => 17 is wrong! pt1 => pt2 + 3 is also wrong!

! DEREFERENCING: when a pointer appears as an alias to a variable it is automatically dereferences; that is, the value of the target is used rather than the
! pointer itself. For a pointer to be dereferenced in this way requires that it be associated with a target. Pointers are automatically dereferenced when
! they appear as part of an expression or in I/O statements; see examples above

! POINTER ASSOCIATION STATUS
! Pointers may be in one of 3 possible states:
! 1. associated - when pointing to a valid target. Becomes the status of an undefined pointer as a result of pointer assignment (=>)
! 2. disassociated - the rseult of a NULLIFY statement
! 3. undefined - the initial state on declaration. Note: it is an error to test the status of an undefined pointer, therefore it is good practise to nullify
! all pointers that are not immediately associated with a target after declaration

associated(pt4,h) ! Check if pointer associated with target in question. Returns Boolean
nullify(pt3, pt4)
associated(pt1, pt2, pt3, pt4) ! Check status of a pointer or list of pointers

! DYNAMIC STORAGE
! As well as pointing to existing variables which have the TARGET attribute, pointers may be associated with blocks of dynamic memory. THis memory is allocated through the
! ALLOCATE statement which creates an unnamed variable or array of the specified size, and with the data type, rank, etc. of the pointer:

real, pointer :: pdyn, padyn(:)
integer, parameter :: n = 100
allocate(pdyn, padyn(n))
deallocate(pdyn, padyn(n))
! In the above, pdyn points to an area of dynamic memory that can hold a single, real number and padyn points to a block of dynamic memory large enough to store 100 real
! numbers. When the memory is no longer required it may be deallocated using the DEALLOCATE statement. In this respect pointers behave very much like allocatabla arrays.

! A common error arises from allocating dynamic storage to the pointer and then re-assigning the pointer to another target; this causes a memory leak. Using NULLIFY(pt) after
! an ALLOCATE(pt(n)) statement means that, since the pointer is the only way to reference the allocated storage, reasssigning the pointer means the allocated storage can
! no longer be released. Therefore all allocated storage should be deallocated before modifying the pointer to it.

! Another error arises when a pointer is assigned to a target, but then the target is removed (by deallocating it or exiting a procedure to which it is local). In this case
! the pointer is left 'dangling'...
real, pointer :: pdyn2
allocate(pdyn, pdyn2)
pdyn2 => pdyn
deallocate(pdyn) ! Wrong!
! in the above, pdyn2 points to the storage allocated to pdyn, however when that storage is deallocated pdyn2 do longer has a valid target and its state becomes undefined.
! In this case dereferencing pdyn2 would produce unpredictable results. So make sure that all pointers to a defunked target are nullified!

! ARRAY POINTERS
! Pointers may act as dynamic aliases to arrays and array sections, such pointers are called array pointers, and are useful when a particular section is referenced frequently
! and can save copying data. For example:

real, target :: grid(10,10)
real, pointer :: centre(:,:), row(:)

centre => grid(4:7,4:7) ! points to block in the centre of the grid
row => grid(9,:) ! points to penultimate row in the grid

! An array pointer can be associated with the whole array or just a section. The size and extent of an array pointer may change as required, just as with allocatable arrays e.g.:
centre => grid(5:5,5:6) ! inner 4 elements of old centre
! Note, an array pointer need not be deallocated before its extent or bounds are redefined:
integer, target :: list(-5:5)
integer, pointer :: pt5(:)
pt5 => list ! note bounds of pt5; pt5(-5:5). i.e. when a pointer is aliased with an array, the array pointer takes its extent from the target array
pt5 => list(:) ! as in prev statement, pt5 points to list, but now with bounds pt5(1:11). i.e. when an array pointer is aliased to an array section (even if that section covers
! the whole array), its lower bound in each dimension is 1. So, in this example statement, pt(1) is aliased to list(-5), and so on
pt5 => list(1:5:2) ! now bounds of pt5 are pt5(1:3), corresponding elems are (i.e. pt5(1), pt5(2) & pt5(3) are aliased to): list(1), list(3) & list(5), i.e. elems 7, 9 & 11 of list

! DERIVED DATA TYPES
! Pointers may be a component of a derived data type. They can take the place of allocatable arrays withing a derived data type, or act as pointers to other objects, incl
! other derived data types. The dynamic nature of pointer arrays can provide varying amounts of storage for a derived data type:
type datatype
real, pointer :: a(:)
end type datatype

integer :: n
type(datatype) :: event(3)
allocate(n(5))
do i = 1,3
    n = 2*i
    allocate(event(i)%a(n))
end do
do i=1,3
    deallocate(event(i)%a)
end do
! Note that the number of values n in the arrays a vary between different instances i of event (3 total stored in the array 'event')

! Derived data type example - linked lists
! Pointers may point to other members of the same data type, and in this way create 'linked lists':
type node
real :: item
type(node), pointer :: next
end type node
! This derived data type contains a single object item (the data in the list) and a pointer "next" to another instance of node. Note the recursion-like property in the
! declaration allowing the pointer to reference its own data type

! IMPLEMENTING A LINKED LIST
type(node), pointer :: firstnode
type(node), pointer :: anode ! used to dynamically change its target during runtime

anode => firstnode
anode%item ! now anode can access data of the first element
anode => anode%next ! used to access the next element
anode%item ! now anode can access data of the second element
! and so on....

! For real this time:
allocate(firstnode, STAT=AllocStat)
firstnode%item = 1.0
anode => firstnode
do i =2,10
    allocate(anode%next)
    anode%next%item = float(i)
    anode => anode%next
end do
nullify(anode%next) ! for the last elem, the "next" pointer has not been linked with an elem. To make sure that it does not point to a random part in the memory, we nullify it

!POINTER ARGUMENTS
!Just like other data types, pointers may be passed as arguments to procedures. There are however a few points to remember when using pointers as actual or dummy arguments:
! - as with other variables, actual & dummy arguments must have the same data type and rank. Dummy arguments that are a pointer may not have the INTENT attribute, since it
!   would be unclear whether the intent would refer to the pointer itself or the associated target
! - pointer arguments to external procedures require INTERFACE blocks
! When both the actual and dummy arguments are pointers, the target (if there is one) and association status is passed on call and again on return. It is important to ensure
! that a target remains valid when returning from a procedure (i.e. the target is not a local procedure variable), otherwise the pointer is left 'dangling'
! When the actual argument is a pointer and the corresponding dummy argument is not, the pointer is dereferenced and it is the target that is copied to the dummy argument.
! On return the target takes the value of the dummy argument. This requires the actual argument to be associated with a target when the procedure is referenced.
! Example:

program prog
interface
subroutine suba(a)
real, pointer :: a(:)
end subroutine suba
end interface
real, pointer :: pt(:)
real, target :: somedata(100)

pt => somedata
call suba(pt)
call subb (pt)

contains
subroutine subb(b) ! internal subroutine. Dummy argument 'b' is not a pointer
real, dimension(:) :: b ! assumed shape of 100
end subroutine subb
end program prog
subroutine suba(a) ! external subroutine, hence required interface block above. Dummy argument 'a' is a pointer
real, pointer :: a(:) ! a points to "somedata"
end subroutine suba
!Note: it is not possible for a non-pointer actual argument to correspond with a pointer dummy argument

!POINTER FUNCTIONS
!Functions may return pointers as their result. This is most useful when the size of the result depends on the function's calculation. Note that:
! - the result must have the POINTER attribute
! - the returning function must have a valid target or have been nullified (i.e. cannot be in an 'undefined' state)
! - pointer results from external procedures require INTERFACE blocks (just as pointer arguments of external subroutines require that subroutine be defined in a
!   INTERFACE block)

!example:
interface
function max_row(a)
real, target :: a(:,:)
real, pointer :: max_row(:)
end function max_row
end interface

real, target :: a(3,3)
real, pointer :: p(:)

pt => max_row(a)

function max_row(a) ! external function
real, target :: a(:, :)
real, pointer :: max_row(:) ! function result (with pointer attribute, as required)
integer :: location(2)
location = maxloc(a) ! row and column of max value
max_row => a(location(1),:) ! pointer to max row
end function max_row
! In the above, the external function max_row returns the row of a matrix containing the largest value. The pointer result is only allowed to point to the dummy argument
! "a" because it is declared as a target (otherwise it would have been a local array and left the pointer dangling on return). Notice the function result is used on the
! right hand side of a pointer assignment statement. A pointer result may be used as part of an expression in which case it must be associated with a target.

! finish up
deallocate(g, pdyn2)

end program main
