#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon cell libspe2)
  (export spe_program_handle_t
          spe_stop_info_t
          spe_event_data_t
          spe_event_unit_t
          SPE_MSSYNC_AREA
          SPE_MFC_COMMAND_AREA
          SPE_CONTROL_AREA
          SPE_SIG_NOTIFY_1_AREA
          SPE_SIG_NOTIFY_2_AREA
          SPE_CFG_SIGNOTIFY1_OR
          SPE_CFG_SIGNOTIFY2_OR
          SPE_MAP_PS
          SPE_ISOLATE
          SPE_ISOLATE_EMULATE
          SPE_EVENTS_ENABLE
          SPE_AFFINITY_MEMORY
          SPE_NOSCHED
          SPE_EXIT
          SPE_STOP_AND_SIGNAL
          SPE_RUNTIME_ERROR
          SPE_RUNTIME_EXCEPTION
          SPE_RUNTIME_FATAL
          SPE_CALLBACK_ERROR
          SPE_ISOLATION_ERROR
          SPE_SPU_STOPPED_BY_STOP
          SPE_SPU_HALT
          SPE_SPU_WAITING_ON_CHANNEL
          SPE_SPU_SINGLE_STEP
          SPE_SPU_INVALID_INSTR
          SPE_SPU_INVALID_CHANNEL
          SPE_DMA_ALIGNMENT
          SPE_SPE_ERROR
          SPE_DMA_SEGMENTATION
          SPE_DMA_STORAGE
          SPE_INVALID_DMA
          SPE_EVENT_OUT_INTR_MBOX
          SPE_EVENT_IN_MBOX
          SPE_EVENT_TAG_GROUP
          SPE_EVENT_SPE_STOPPED
          SPE_EVENT_ALL_EVENTS
          SPE_MBOX_ALL_BLOCKING
          SPE_MBOX_ANY_BLOCKING
          SPE_MBOX_ANY_NONBLOCKING
          SPE_TAG_ALL
          SPE_TAG_ANY
          SPE_TAG_IMMEDIATE
          SPE_DEFAULT_ENTRY
          SPE_RUN_USER_REGS
          SPE_NO_CALLBACKS
          SPE_CALLBACK_NEW
          SPE_CALLBACK_UPDATE
          SPE_COUNT_PHYSICAL_CPU_NODES
          SPE_COUNT_PHYSICAL_SPES
          SPE_COUNT_USABLE_SPES
          SPE_SIG_NOTIFY_REG_1
          SPE_SIG_NOTIFY_REG_2
          spe_context_create
          spe_context_create_affinity
          spe_context_destroy
          spe_gang_context_create
          spe_gang_context_destroy
          spe_image_open
          spe_image_close
          spe_load_program
          spe_context_run
          spe_stop_info_read
          spe_event_handler_create
          spe_event_handler_destroy
          spe_event_handler_register
          spe_event_handler_deregister
          spe_event_wait
          spe_mfcio_put
          spe_mfcio_putb
          spe_mfcio_putf
          spe_mfcio_get
          spe_mfcio_getb
          spe_mfcio_getf
          spe_mfcio_tag_status_read
          spe_out_mbox_read
          spe_out_mbox_status
          spe_in_mbox_write
          spe_in_mbox_status
          spe_out_intr_mbox_read
          spe_out_intr_mbox_status
          spe_mssync_start
          spe_mssync_status
          spe_signal_write
          spe_ls_area_get
          spe_ls_size_get
          spe_ps_area_get
          spe_callback_handler_register
          spe_callback_handler_deregister
          spe_callback_handler_query
          spe_cpu_info_get)

  (import (rnrs)
          (ypsilon ffi))

  (define lib-name "libspe2.so")
  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-c-typedef spe_program_handle_t
    (struct (unsigned-int handle-size)
            (void* elf_image)
            (void* toe_shadow)))

  (define-c-typedef spe_stop_info_t
    (struct (unsigned-int stop_reason)
            (int64_t code)
            (int spu_status)))

  (define-c-typedef spe_event_data_t
    (struct (void* ptr)
            (unsigned-int u32)
            (uint64_t u64)))

  (define-c-typedef spe_event_unit_t
    (struct (unsigned-int events)
            (void* spe)
            (spe_event_data_t data)
            (uint64_t u64)))

  (define-c-enum SPE_MSSYNC_AREA
                 SPE_MFC_COMMAND_AREA
                 SPE_CONTROL_AREA
                 SPE_SIG_NOTIFY_1_AREA
                 SPE_SIG_NOTIFY_2_AREA)

  (define SPE_CFG_SIGNOTIFY1_OR        #x00000010)
  (define SPE_CFG_SIGNOTIFY2_OR        #x00000020)
  (define SPE_MAP_PS                   #x00000040)
  (define SPE_ISOLATE                  #x00000080)
  (define SPE_ISOLATE_EMULATE          #x00000100)
  (define SPE_EVENTS_ENABLE            #x00001000)
  (define SPE_AFFINITY_MEMORY          #x00002000)
  (define SPE_NOSCHED                  #x00004000)
  (define SPE_EXIT                     1)
  (define SPE_STOP_AND_SIGNAL          2)
  (define SPE_RUNTIME_ERROR            3)
  (define SPE_RUNTIME_EXCEPTION        4)
  (define SPE_RUNTIME_FATAL            5)
  (define SPE_CALLBACK_ERROR           6)
  (define SPE_ISOLATION_ERROR          7)
  (define SPE_SPU_STOPPED_BY_STOP      #x02)
  (define SPE_SPU_HALT                 #x04)
  (define SPE_SPU_WAITING_ON_CHANNEL   #x08)
  (define SPE_SPU_SINGLE_STEP          #x10)
  (define SPE_SPU_INVALID_INSTR        #x20)
  (define SPE_SPU_INVALID_CHANNEL      #x40)
  (define SPE_DMA_ALIGNMENT            #x0008)
  (define SPE_SPE_ERROR                #x0010)
  (define SPE_DMA_SEGMENTATION         #x0020)
  (define SPE_DMA_STORAGE              #x0040)
  (define SPE_INVALID_DMA              #x0800)
#;(define SIGSPE SIGURG)
  (define SPE_EVENT_OUT_INTR_MBOX      #x00000001)
  (define SPE_EVENT_IN_MBOX            #x00000002)
  (define SPE_EVENT_TAG_GROUP          #x00000004)
  (define SPE_EVENT_SPE_STOPPED        #x00000008)
  (define SPE_EVENT_ALL_EVENTS         (+ SPE_EVENT_OUT_INTR_MBOX
                                          SPE_EVENT_IN_MBOX
                                          SPE_EVENT_TAG_GROUP
                                          SPE_EVENT_SPE_STOPPED))
  (define SPE_MBOX_ALL_BLOCKING        1)
  (define SPE_MBOX_ANY_BLOCKING        2)
  (define SPE_MBOX_ANY_NONBLOCKING     3)
  (define SPE_TAG_ALL                  1)
  (define SPE_TAG_ANY                  2)
  (define SPE_TAG_IMMEDIATE            3)
  (define SPE_DEFAULT_ENTRY            4294967295)
  (define SPE_RUN_USER_REGS            #x00000001)
  (define SPE_NO_CALLBACKS             #x00000002)
  (define SPE_CALLBACK_NEW             1)
  (define SPE_CALLBACK_UPDATE          2)
  (define SPE_COUNT_PHYSICAL_CPU_NODES 1)
  (define SPE_COUNT_PHYSICAL_SPES      2)
  (define SPE_COUNT_USABLE_SPES        3)
  (define SPE_SIG_NOTIFY_REG_1         #x0001)
  (define SPE_SIG_NOTIFY_REG_2         #x0002)

  ;; spe_context_ptr_t spe_context_create(unsigned int flags, spe_gang_context_ptr_t gang)
  (define-function void* spe_context_create (unsigned-int void*))

  ;; spe_context_ptr_t spe_context_create_affinity(unsigned int flags, spe_context_ptr_t affinity_neighbor, spe_gang_context_ptr_t gang)
  (define-function void* spe_context_create_affinity (unsigned-int void* void*))

  ;; int spe_context_destroy (spe_context_ptr_t spe)
  (define-function int spe_context_destroy (void*))

  ;; spe_gang_context_ptr_t spe_gang_context_create (unsigned int flags)
  (define-function void* spe_gang_context_create (unsigned-int))

  ;; int spe_gang_context_destroy (spe_gang_context_ptr_t gang)
  (define-function int spe_gang_context_destroy (void*))

  ;; spe_program_handle_t * spe_image_open (const char *filename)
  (define-function void* spe_image_open (char*))

  ;; int spe_image_close (spe_program_handle_t *program)
  (define-function int spe_image_close (void*))

  ;; int spe_program_load (spe_context_ptr_t spe, spe_program_handle_t *program)
  (define-function int spe_load_program (void* void*))

  ;; int spe_context_run (spe_context_ptr_t spe, unsigned int *entry, unsigned int runflags, void *argp, void *envp, spe_stop_info_t *stopinfo)
  (define-function int spe_context_run (void* void* unsigned-int void* void* void*))

  ;; int spe_stop_info_read (spe_context_ptr_t spe, spe_stop_info_t *stopinfo)
  (define-function int spe_stop_info_read (void* void*))

  ;; spe_event_handler_ptr_t spe_event_handler_create(void)
  (define-function void* spe_event_handler_create ())

  ;; int spe_event_handler_destroy (spe_event_handler_ptr_t evhandler)
  (define-function int spe_event_handler_destroy (void*))

  ;; int spe_event_handler_register(spe_event_handler_ptr_t evhandler, spe_event_unit_t *event)
  (define-function int spe_event_handler_register (void* void*))

  ;; int spe_event_handler_deregister(spe_event_handler_ptr_t evhandler, spe_event_unit_t *event)
  (define-function int spe_event_handler_deregister (void* void*))

  ;; int spe_event_wait(spe_event_handler_ptr_t evhandler, spe_event_unit_t *events, int max_events, int timeout)
  (define-function int spe_event_wait (void* void* int int))

  ;; int spe_mfcio_put (spe_context_ptr_t spe, unsigned int ls, void *ea, unsigned int size, unsigned int tag, unsigned int tid, unsigned int rid)
  (define-function int spe_mfcio_put (void* unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; int spe_mfcio_putb (spe_context_ptr_t spe, unsigned int ls, void *ea, unsigned int size, unsigned int tag, unsigned int tid, unsigned int rid)
  (define-function int spe_mfcio_putb (void* unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; int spe_mfcio_putf (spe_context_ptr_t spe, unsigned int ls, void *ea, unsigned int size, unsigned int tag, unsigned int tid, unsigned int rid)
  (define-function int spe_mfcio_putf (void* unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; int spe_mfcio_get (spe_context_ptr_t spe, unsigned int ls, void *ea, unsigned int size, unsigned int tag, unsigned int tid, unsigned int rid)
  (define-function int spe_mfcio_get (void* unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; int spe_mfcio_getb (spe_context_ptr_t spe, unsigned int ls, void *ea, unsigned int size, unsigned int tag, unsigned int tid, unsigned int rid)
  (define-function int spe_mfcio_getb (void* unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; int spe_mfcio_getf (spe_context_ptr_t spe, unsigned int ls, void *ea, unsigned int size, unsigned int tag, unsigned int tid, unsigned int rid)
  (define-function int spe_mfcio_getf (void* unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; int spe_mfcio_tag_status_read(spe_context_ptr_t spe, unsigned int mask, unsigned int behavior, unsigned int *tag_status)
  (define-function int spe_mfcio_tag_status_read (void* unsigned-int unsigned-int void*))

  ;; int spe_out_mbox_read (spe_context_ptr_t spe, unsigned int *mbox_data, int count)
  (define-function int spe_out_mbox_read (void* void* int))

  ;; int spe_out_mbox_status (spe_context_ptr_t spe)
  (define-function int spe_out_mbox_status (void*))

  ;; int spe_in_mbox_write (spe_context_ptr_t spe, unsigned int *mbox_data, int count, unsigned int behavior)
  (define-function int spe_in_mbox_write (void* void* int unsigned-int))

  ;; int spe_in_mbox_status (spe_context_ptr_t spe)
  (define-function int spe_in_mbox_status (void*))

  ;; int spe_out_intr_mbox_read (spe_context_ptr_t spe, unsigned int *mbox_data, int count, unsigned int behavior)
  (define-function int spe_out_intr_mbox_read (void* void* int unsigned-int))

  ;; int spe_out_intr_mbox_status (spe_context_ptr_t spe)
  (define-function int spe_out_intr_mbox_status (void*))

  ;; int spe_mssync_start(spe_context_ptr_t spe)
  (define-function int spe_mssync_start (void*))

  ;; int spe_mssync_status(spe_context_ptr_t spe)
  (define-function int spe_mssync_status (void*))

  ;; int spe_signal_write (spe_context_ptr_t spe, unsigned int signal_reg, unsigned int data)
  (define-function int spe_signal_write (void* unsigned-int unsigned-int))

  ;; void * spe_ls_area_get (spe_context_ptr_t spe)
  (define-function void* spe_ls_area_get (void*))

  ;; int spe_ls_size_get (spe_context_ptr_t spe)
  (define-function int spe_ls_size_get (void*))

  ;; void * spe_ps_area_get (spe_context_ptr_t spe, enum ps_area area)
  (define-function void* spe_ps_area_get (void* int))

  ;; int spe_callback_handler_register (void *handler, unsigned int callnum, unsigned int mode)
  (define-function int spe_callback_handler_register (void* unsigned-int unsigned-int))

  ;; int spe_callback_handler_deregister (unsigned int callnum)
  (define-function int spe_callback_handler_deregister (unsigned-int))

  ;; void * spe_callback_handler_query(unsigned int callnum)
  (define-function void* spe_callback_handler_query (unsigned-int))

  ;; int spe_cpu_info_get(int info_requested, int cpu_node)
  (define-function int spe_cpu_info_get (int int))

  ) ;[end]
