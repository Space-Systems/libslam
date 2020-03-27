
! ENTER_* and EXIT_* macros
!
! These two macro families are to be used as first or last statement of
! procedures. This way, some helpful stuff is done, especially for debug builds which
! are specified by setting the DEBUG_BUILD macro (done via CMake). The
! helpful stuff is the following:
!
! All builds:
! * "tehl" (tracking, error handling and logging) is available via a local variable
!   named "t". Especially, use logging!
!
! Debug builds:
! * The local lifecycle_helper object is dealt with
! * Tracing is done which lets you see which procedure produced what log statement
! * A stack trace is shown upon errors
! * If error handling has been set so
!   that all procedures should return immediately, exactly this behaviour is executed.
!
! Usage: Use the ENTER_* and EXIT_* MACRO which fits the procedure (subroutine or
! function) that you are looking at. Use the macro that is nearest to the procedure
! type. Note that they normally have corresponding names
! ("something_finalize_connector" would be FINALIZE_CONNECTOR procedure, for example).
!
! ENTER_STATIC_PROCEDURE: Is a class-specific procedure meaning a procedure that doesn't work on
! one specific object, but on the class level.
! ENTER_STARTUP_PROCEDURE: A procedure where error handling has not been initialized before the
! procedure is executed.
! ENTER_CREATE_PROCEDURE: A procedure which delivers a newly created object of a class. Note that
! there is no CREATE_CONNECTOR-specific macro. Use this CREATE macro instead.
! ENTER_FINALIZE_PROCEDURE: A procedure which destroys an object
! ENTER_FINALIZE_CONNECTOR_PROCEDURE: A procedure used in plugins. They make up the connection
! between the specific plugin and callers of the plugin from extern (LUCA core,
! for example).
! ENTER_FINALIZE_INTERNAL_PROCEDURE: A procedure called automatically by the system when an
! object is destroyed.
! ENTER_NORMAL_PROCEDURE: If no specific macro fits, this is the one to use
! EXIT_PROCEDURE: The only exit procedure macro
! @copyright Apache License 2.0

#ifdef DEBUG_BUILD

#define ENTER_EXIT_COMMON_DECLARATIONS(procedure_name) ;\
  character(len = *), parameter :: current_procedure_name = procedure_name ;\
  class(tehl_type), pointer :: t => null() ;\
  integer, parameter :: file_line_number_as_string_length__ = 4 ;\
  character(len = file_line_number_as_string_length__) :: file_line_as_string__ ;\
  character(len = len(trim(current_procedure_name)) + file_line_number_as_string_length__ + 7) :: checkin_name

#define ENTER_EXIT_CHECKIN_STUFF ;\
  write(file_line_as_string__, '(I4.4)') __LINE__ ;\
  checkin_name = current_procedure_name//"[line"//file_line_as_string__//"]" ;\
  t => tehl_create_or_get_singleton() ;\
  if (.not. t%enter_procedure_ok(checkin_name)) then ;\
    return ;\
  end if

#define ENTER_STATIC_PROCEDURE(procedure_name) ;\
  ENTER_EXIT_COMMON_DECLARATIONS(procedure_name) ;\
  ENTER_EXIT_CHECKIN_STUFF

#define ENTER_STARTUP_PROCEDURE(procedure_name) ;\
  t => tehl_create_or_get_singleton() ;\
  if (.not. t%enter_procedure_ok(checkin_name)) then ;\
    return ;\
  end if ;\
  this%lifecycle_helper => lifecycle_helper_create_if_non_existent_and_ensure_create_call(this%lifecycle_helper)

#define ENTER_CREATE_PROCEDURE(procedure_name) ;\
  ENTER_EXIT_COMMON_DECLARATIONS(procedure_name) ;\
  ENTER_EXIT_CHECKIN_STUFF

#define ENTER_FINALIZE_CONNECTOR_PROCEDURE(procedure_name) ;\
  ENTER_EXIT_COMMON_DECLARATIONS(procedure_name) ;\
  ENTER_EXIT_CHECKIN_STUFF

#define ENTER_FINALIZE_PROCEDURE(procedure_name) ;\
  ENTER_EXIT_COMMON_DECLARATIONS(procedure_name) ;\
  ENTER_EXIT_CHECKIN_STUFF ;\
  object_to_destroy%lifecycle_helper => lifecycle_helper_create_if_non_existent_and_ensure_create_call(object_to_destroy%lifecycle_helper) ;\
  call object_to_destroy%lifecycle_helper%set_finalize_subroutine_called()

#define ENTER_FINALIZE_INTERNAL_PROCEDURE(procedure_name) ;\
  ENTER_EXIT_COMMON_DECLARATIONS(procedure_name) ;\
  ENTER_EXIT_CHECKIN_STUFF ;\
  this%lifecycle_helper => lifecycle_helper_create_if_non_existent_and_ensure_final_call(this%lifecycle_helper)

#define ENTER_NORMAL_PROCEDURE(procedure_name) ;\
  ENTER_EXIT_COMMON_DECLARATIONS(procedure_name) ;\
  ENTER_EXIT_CHECKIN_STUFF ;\
  this%lifecycle_helper => lifecycle_helper_create_if_non_existent_and_ensure_create_call(this%lifecycle_helper)


#define EXIT_PROCEDURE() ;\
  call t%exit_procedure(checkin_name)
#else


#define ENTER_STARTUP_PROCEDURE(procedure_name) ;\
  t => tehl_create_or_get_singleton()
#define ENTER_STATIC_PROCEDURE(procedure_name) ;\
  class(tehl_type), pointer :: t => null() ;\
  t => tehl_create_or_get_singleton()
#define ENTER_CREATE_PROCEDURE(procedure_name) ;\
  class(tehl_type), pointer :: t => null() ;\
  t => tehl_create_or_get_singleton()
#define ENTER_FINALIZE_PROCEDURE(procedure_name) ;\
  class(tehl_type), pointer :: t => null() ;\
  t => tehl_create_or_get_singleton()
#define ENTER_FINALIZE_CONNECTOR_PROCEDURE(procedure_name) ;\
  class(tehl_type), pointer :: t => null() ;\
  t => tehl_create_or_get_singleton()
#define ENTER_FINALIZE_INTERNAL_PROCEDURE(procedure_name) ;\
  class(tehl_type), pointer :: t => null() ;\
  t => tehl_create_or_get_singleton()
#define ENTER_NORMAL_PROCEDURE(procedure_name) ;\
  class(tehl_type), pointer :: t => null() ;\
  t => tehl_create_or_get_singleton()
#define EXIT_PROCEDURE() ! NOTHING

#endif
! #ifdef DEBUG_BUILD
! free line at the end important.
!If a "module ..." line follows directly after inclusion of this file,
!the MOD file for that module won't be generated. Why? I don't know!
