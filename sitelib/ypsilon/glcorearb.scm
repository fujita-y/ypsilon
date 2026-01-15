#!nobacktrace
;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon glcorearb)
  (export glCullFace
          glFrontFace
          glHint
          glLineWidth
          glPointSize
          glPolygonMode
          glScissor
          glTexParameterf
          glTexParameterfv
          glTexParameteri
          glTexParameteriv
          glTexImage1D
          glTexImage2D
          glDrawBuffer
          glClear
          glClearColor
          glClearStencil
          glClearDepth
          glStencilMask
          glColorMask
          glDepthMask
          glDisable
          glEnable
          glFinish
          glFlush
          glBlendFunc
          glLogicOp
          glStencilFunc
          glStencilOp
          glDepthFunc
          glPixelStoref
          glPixelStorei
          glReadBuffer
          glReadPixels
          glGetBooleanv
          glGetDoublev
          glGetError
          glGetFloatv
          glGetIntegerv
          glGetTexImage
          glGetTexParameterfv
          glGetTexParameteriv
          glGetTexLevelParameterfv
          glGetTexLevelParameteriv
          glIsEnabled
          glDepthRange
          glViewport
          glDrawArrays
          glDrawElements
          glGetPointerv
          glPolygonOffset
          glCopyTexImage1D
          glCopyTexImage2D
          glCopyTexSubImage1D
          glCopyTexSubImage2D
          glTexSubImage1D
          glTexSubImage2D
          glBindTexture
          glDeleteTextures
          glGenTextures
          glIsTexture
          glDrawRangeElements
          glTexImage3D
          glTexSubImage3D
          glCopyTexSubImage3D
          glActiveTexture
          glSampleCoverage
          glCompressedTexImage3D
          glCompressedTexImage2D
          glCompressedTexImage1D
          glCompressedTexSubImage3D
          glCompressedTexSubImage2D
          glCompressedTexSubImage1D
          glGetCompressedTexImage
          glBlendFuncSeparate
          glMultiDrawArrays
          glMultiDrawElements
          glPointParameterf
          glPointParameterfv
          glPointParameteri
          glPointParameteriv
          glBlendColor
          glBlendEquation
          glGenQueries
          glDeleteQueries
          glIsQuery
          glBeginQuery
          glEndQuery
          glGetQueryiv
          glGetQueryObjectiv
          glGetQueryObjectuiv
          glBindBuffer
          glDeleteBuffers
          glGenBuffers
          glIsBuffer
          glBufferData
          glBufferSubData
          glGetBufferSubData
          glUnmapBuffer
          glGetBufferParameteriv
          glGetBufferPointerv
          glBlendEquationSeparate
          glDrawBuffers
          glStencilOpSeparate
          glStencilFuncSeparate
          glStencilMaskSeparate
          glAttachShader
          glBindAttribLocation
          glCompileShader
          glCreateProgram
          glCreateShader
          glDeleteProgram
          glDeleteShader
          glDetachShader
          glDisableVertexAttribArray
          glEnableVertexAttribArray
          glGetActiveAttrib
          glGetActiveUniform
          glGetAttachedShaders
          glGetAttribLocation
          glGetProgramiv
          glGetProgramInfoLog
          glGetShaderiv
          glGetShaderInfoLog
          glGetShaderSource
          glGetUniformLocation
          glGetUniformfv
          glGetUniformiv
          glGetVertexAttribdv
          glGetVertexAttribfv
          glGetVertexAttribiv
          glGetVertexAttribPointerv
          glIsProgram
          glIsShader
          glLinkProgram
          glShaderSource
          glUseProgram
          glUniform1f
          glUniform2f
          glUniform3f
          glUniform4f
          glUniform1i
          glUniform2i
          glUniform3i
          glUniform4i
          glUniform1fv
          glUniform2fv
          glUniform3fv
          glUniform4fv
          glUniform1iv
          glUniform2iv
          glUniform3iv
          glUniform4iv
          glUniformMatrix2fv
          glUniformMatrix3fv
          glUniformMatrix4fv
          glValidateProgram
          glVertexAttrib1d
          glVertexAttrib1dv
          glVertexAttrib1f
          glVertexAttrib1fv
          glVertexAttrib1s
          glVertexAttrib1sv
          glVertexAttrib2d
          glVertexAttrib2dv
          glVertexAttrib2f
          glVertexAttrib2fv
          glVertexAttrib2s
          glVertexAttrib2sv
          glVertexAttrib3d
          glVertexAttrib3dv
          glVertexAttrib3f
          glVertexAttrib3fv
          glVertexAttrib3s
          glVertexAttrib3sv
          glVertexAttrib4Nbv
          glVertexAttrib4Niv
          glVertexAttrib4Nsv
          glVertexAttrib4Nub
          glVertexAttrib4Nubv
          glVertexAttrib4Nuiv
          glVertexAttrib4Nusv
          glVertexAttrib4bv
          glVertexAttrib4d
          glVertexAttrib4dv
          glVertexAttrib4f
          glVertexAttrib4fv
          glVertexAttrib4iv
          glVertexAttrib4s
          glVertexAttrib4sv
          glVertexAttrib4ubv
          glVertexAttrib4uiv
          glVertexAttrib4usv
          glVertexAttribPointer
          glUniformMatrix2x3fv
          glUniformMatrix3x2fv
          glUniformMatrix2x4fv
          glUniformMatrix4x2fv
          glUniformMatrix3x4fv
          glUniformMatrix4x3fv
          glColorMaski
          glEnablei
          glDisablei
          glIsEnabledi
          glBeginTransformFeedback
          glEndTransformFeedback
          glBindBufferRange
          glBindBufferBase
          glTransformFeedbackVaryings
          glGetTransformFeedbackVarying
          glClampColor
          glBeginConditionalRender
          glEndConditionalRender
          glVertexAttribIPointer
          glGetVertexAttribIiv
          glGetVertexAttribIuiv
          glVertexAttribI1i
          glVertexAttribI2i
          glVertexAttribI3i
          glVertexAttribI4i
          glVertexAttribI1ui
          glVertexAttribI2ui
          glVertexAttribI3ui
          glVertexAttribI4ui
          glVertexAttribI1iv
          glVertexAttribI2iv
          glVertexAttribI3iv
          glVertexAttribI4iv
          glVertexAttribI1uiv
          glVertexAttribI2uiv
          glVertexAttribI3uiv
          glVertexAttribI4uiv
          glVertexAttribI4bv
          glVertexAttribI4sv
          glVertexAttribI4ubv
          glVertexAttribI4usv
          glGetUniformuiv
          glBindFragDataLocation
          glGetFragDataLocation
          glUniform1ui
          glUniform2ui
          glUniform3ui
          glUniform4ui
          glUniform1uiv
          glUniform2uiv
          glUniform3uiv
          glUniform4uiv
          glTexParameterIiv
          glTexParameterIuiv
          glGetTexParameterIiv
          glGetTexParameterIuiv
          glClearBufferiv
          glClearBufferuiv
          glClearBufferfv
          glClearBufferfi
          glIsRenderbuffer
          glBindRenderbuffer
          glDeleteRenderbuffers
          glGenRenderbuffers
          glRenderbufferStorage
          glGetRenderbufferParameteriv
          glIsFramebuffer
          glBindFramebuffer
          glDeleteFramebuffers
          glGenFramebuffers
          glCheckFramebufferStatus
          glFramebufferTexture1D
          glFramebufferTexture2D
          glFramebufferTexture3D
          glFramebufferRenderbuffer
          glGetFramebufferAttachmentParameteriv
          glGenerateMipmap
          glBlitFramebuffer
          glRenderbufferStorageMultisample
          glFramebufferTextureLayer
          glFlushMappedBufferRange
          glBindVertexArray
          glDeleteVertexArrays
          glGenVertexArrays
          glIsVertexArray
          glDrawArraysInstanced
          glDrawElementsInstanced
          glTexBuffer
          glPrimitiveRestartIndex
          glCopyBufferSubData
          glGetUniformIndices
          glGetActiveUniformsiv
          glGetActiveUniformName
          glGetUniformBlockIndex
          glGetActiveUniformBlockiv
          glGetActiveUniformBlockName
          glUniformBlockBinding
          glDrawElementsBaseVertex
          glDrawRangeElementsBaseVertex
          glDrawElementsInstancedBaseVertex
          glMultiDrawElementsBaseVertex
          glProvokingVertex
          glFenceSync
          glIsSync
          glDeleteSync
          glClientWaitSync
          glWaitSync
          glGetInteger64v
          glGetSynciv
          glGetBufferParameteri64v
          glFramebufferTexture
          glTexImage2DMultisample
          glTexImage3DMultisample
          glGetMultisamplefv
          glSampleMaski
          glBindFragDataLocationIndexed
          glGetFragDataIndex
          glGenSamplers
          glDeleteSamplers
          glIsSampler
          glBindSampler
          glSamplerParameteri
          glSamplerParameteriv
          glSamplerParameterf
          glSamplerParameterfv
          glSamplerParameterIiv
          glSamplerParameterIuiv
          glGetSamplerParameteriv
          glGetSamplerParameterIiv
          glGetSamplerParameterfv
          glGetSamplerParameterIuiv
          glQueryCounter
          glGetQueryObjecti64v
          glGetQueryObjectui64v
          glVertexAttribDivisor
          glVertexAttribP1ui
          glVertexAttribP1uiv
          glVertexAttribP2ui
          glVertexAttribP2uiv
          glVertexAttribP3ui
          glVertexAttribP3uiv
          glVertexAttribP4ui
          glVertexAttribP4uiv
          glMinSampleShading
          glBlendEquationi
          glBlendEquationSeparatei
          glBlendFunci
          glBlendFuncSeparatei
          glDrawArraysIndirect
          glDrawElementsIndirect
          glUniform1d
          glUniform2d
          glUniform3d
          glUniform4d
          glUniform1dv
          glUniform2dv
          glUniform3dv
          glUniform4dv
          glUniformMatrix2dv
          glUniformMatrix3dv
          glUniformMatrix4dv
          glUniformMatrix2x3dv
          glUniformMatrix2x4dv
          glUniformMatrix3x2dv
          glUniformMatrix3x4dv
          glUniformMatrix4x2dv
          glUniformMatrix4x3dv
          glGetUniformdv
          glGetSubroutineUniformLocation
          glGetSubroutineIndex
          glGetActiveSubroutineUniformiv
          glGetActiveSubroutineUniformName
          glGetActiveSubroutineName
          glUniformSubroutinesuiv
          glGetUniformSubroutineuiv
          glGetProgramStageiv
          glPatchParameteri
          glPatchParameterfv
          glBindTransformFeedback
          glDeleteTransformFeedbacks
          glGenTransformFeedbacks
          glIsTransformFeedback
          glPauseTransformFeedback
          glResumeTransformFeedback
          glDrawTransformFeedback
          glDrawTransformFeedbackStream
          glBeginQueryIndexed
          glEndQueryIndexed
          glGetQueryIndexediv
          glReleaseShaderCompiler
          glShaderBinary
          glGetShaderPrecisionFormat
          glDepthRangef
          glClearDepthf
          glGetProgramBinary
          glProgramBinary
          glProgramParameteri
          glUseProgramStages
          glActiveShaderProgram
          glCreateShaderProgramv
          glBindProgramPipeline
          glDeleteProgramPipelines
          glGenProgramPipelines
          glIsProgramPipeline
          glGetProgramPipelineiv
          glProgramUniform1i
          glProgramUniform1iv
          glProgramUniform1f
          glProgramUniform1fv
          glProgramUniform1d
          glProgramUniform1dv
          glProgramUniform1ui
          glProgramUniform1uiv
          glProgramUniform2i
          glProgramUniform2iv
          glProgramUniform2f
          glProgramUniform2fv
          glProgramUniform2d
          glProgramUniform2dv
          glProgramUniform2ui
          glProgramUniform2uiv
          glProgramUniform3i
          glProgramUniform3iv
          glProgramUniform3f
          glProgramUniform3fv
          glProgramUniform3d
          glProgramUniform3dv
          glProgramUniform3ui
          glProgramUniform3uiv
          glProgramUniform4i
          glProgramUniform4iv
          glProgramUniform4f
          glProgramUniform4fv
          glProgramUniform4d
          glProgramUniform4dv
          glProgramUniform4ui
          glProgramUniform4uiv
          glProgramUniformMatrix2fv
          glProgramUniformMatrix3fv
          glProgramUniformMatrix4fv
          glProgramUniformMatrix2dv
          glProgramUniformMatrix3dv
          glProgramUniformMatrix4dv
          glProgramUniformMatrix2x3fv
          glProgramUniformMatrix3x2fv
          glProgramUniformMatrix2x4fv
          glProgramUniformMatrix4x2fv
          glProgramUniformMatrix3x4fv
          glProgramUniformMatrix4x3fv
          glProgramUniformMatrix2x3dv
          glProgramUniformMatrix3x2dv
          glProgramUniformMatrix2x4dv
          glProgramUniformMatrix4x2dv
          glProgramUniformMatrix3x4dv
          glProgramUniformMatrix4x3dv
          glValidateProgramPipeline
          glGetProgramPipelineInfoLog
          glVertexAttribL1d
          glVertexAttribL2d
          glVertexAttribL3d
          glVertexAttribL4d
          glVertexAttribL1dv
          glVertexAttribL2dv
          glVertexAttribL3dv
          glVertexAttribL4dv
          glVertexAttribLPointer
          glGetVertexAttribLdv
          glViewportArrayv
          glViewportIndexedf
          glViewportIndexedfv
          glScissorArrayv
          glScissorIndexed
          glScissorIndexedv
          glDepthRangeArrayv
          glDepthRangeIndexed
          glDrawArraysInstancedBaseInstance
          glDrawElementsInstancedBaseInstance
          glDrawElementsInstancedBaseVertexBaseInstance
          glGetInternalformativ
          glGetActiveAtomicCounterBufferiv
          glBindImageTexture
          glMemoryBarrier
          glTexStorage1D
          glTexStorage2D
          glTexStorage3D
          glDrawTransformFeedbackInstanced
          glDrawTransformFeedbackStreamInstanced
          glClearBufferData
          glClearBufferSubData
          glDispatchCompute
          glDispatchComputeIndirect
          glCopyImageSubData
          glFramebufferParameteri
          glGetFramebufferParameteriv
          glGetInternalformati64v
          glInvalidateTexSubImage
          glInvalidateTexImage
          glInvalidateBufferSubData
          glInvalidateBufferData
          glInvalidateFramebuffer
          glInvalidateSubFramebuffer
          glMultiDrawArraysIndirect
          glMultiDrawElementsIndirect
          glGetProgramInterfaceiv
          glGetProgramResourceIndex
          glGetProgramResourceName
          glGetProgramResourceiv
          glGetProgramResourceLocation
          glGetProgramResourceLocationIndex
          glShaderStorageBlockBinding
          glTexBufferRange
          glTexStorage2DMultisample
          glTexStorage3DMultisample
          glTextureView
          glBindVertexBuffer
          glVertexAttribFormat
          glVertexAttribIFormat
          glVertexAttribLFormat
          glVertexAttribBinding
          glVertexBindingDivisor
          glDebugMessageControl
          glDebugMessageInsert
          glDebugMessageCallback
          glGetDebugMessageLog
          glPushDebugGroup
          glPopDebugGroup
          glObjectLabel
          glGetObjectLabel
          glObjectPtrLabel
          glGetObjectPtrLabel
          glBufferStorage
          glClearTexImage
          glClearTexSubImage
          glBindBuffersBase
          glBindBuffersRange
          glBindTextures
          glBindSamplers
          glBindImageTextures
          glBindVertexBuffers
          glClipControl
          glCreateTransformFeedbacks
          glTransformFeedbackBufferBase
          glTransformFeedbackBufferRange
          glGetTransformFeedbackiv
          glCreateBuffers
          glNamedBufferStorage
          glNamedBufferData
          glNamedBufferSubData
          glCopyNamedBufferSubData
          glClearNamedBufferData
          glClearNamedBufferSubData
          glUnmapNamedBuffer
          glFlushMappedNamedBufferRange
          glGetNamedBufferParameteriv
          glGetNamedBufferParameteri64v
          glGetNamedBufferPointerv
          glGetNamedBufferSubData
          glCreateFramebuffers
          glNamedFramebufferRenderbuffer
          glNamedFramebufferParameteri
          glNamedFramebufferTexture
          glNamedFramebufferTextureLayer
          glNamedFramebufferDrawBuffer
          glNamedFramebufferDrawBuffers
          glNamedFramebufferReadBuffer
          glInvalidateNamedFramebufferData
          glInvalidateNamedFramebufferSubData
          glClearNamedFramebufferiv
          glClearNamedFramebufferuiv
          glClearNamedFramebufferfv
          glClearNamedFramebufferfi
          glBlitNamedFramebuffer
          glCheckNamedFramebufferStatus
          glGetNamedFramebufferParameteriv
          glGetNamedFramebufferAttachmentParameteriv
          glCreateRenderbuffers
          glNamedRenderbufferStorage
          glNamedRenderbufferStorageMultisample
          glGetNamedRenderbufferParameteriv
          glCreateTextures
          glTextureBuffer
          glTextureBufferRange
          glTextureStorage1D
          glTextureStorage2D
          glTextureStorage3D
          glTextureStorage2DMultisample
          glTextureStorage3DMultisample
          glTextureSubImage1D
          glTextureSubImage2D
          glTextureSubImage3D
          glCompressedTextureSubImage1D
          glCompressedTextureSubImage2D
          glCompressedTextureSubImage3D
          glCopyTextureSubImage1D
          glCopyTextureSubImage2D
          glCopyTextureSubImage3D
          glTextureParameterf
          glTextureParameterfv
          glTextureParameteri
          glTextureParameterIiv
          glTextureParameterIuiv
          glTextureParameteriv
          glGenerateTextureMipmap
          glBindTextureUnit
          glGetTextureImage
          glGetCompressedTextureImage
          glGetTextureLevelParameterfv
          glGetTextureLevelParameteriv
          glGetTextureParameterfv
          glGetTextureParameterIiv
          glGetTextureParameterIuiv
          glGetTextureParameteriv
          glCreateVertexArrays
          glDisableVertexArrayAttrib
          glEnableVertexArrayAttrib
          glVertexArrayElementBuffer
          glVertexArrayVertexBuffer
          glVertexArrayVertexBuffers
          glVertexArrayAttribBinding
          glVertexArrayAttribFormat
          glVertexArrayAttribIFormat
          glVertexArrayAttribLFormat
          glVertexArrayBindingDivisor
          glGetVertexArrayiv
          glGetVertexArrayIndexediv
          glGetVertexArrayIndexed64iv
          glCreateSamplers
          glCreateProgramPipelines
          glCreateQueries
          glGetQueryBufferObjecti64v
          glGetQueryBufferObjectiv
          glGetQueryBufferObjectui64v
          glGetQueryBufferObjectuiv
          glMemoryBarrierByRegion
          glGetTextureSubImage
          glGetCompressedTextureSubImage
          glGetGraphicsResetStatus
          glGetnCompressedTexImage
          glGetnTexImage
          glGetnUniformdv
          glGetnUniformfv
          glGetnUniformiv
          glGetnUniformuiv
          glReadnPixels
          glTextureBarrier
          glSpecializeShader
          glMultiDrawArraysIndirectCount
          glMultiDrawElementsIndirectCount
          glPolygonOffsetClamp
          glPrimitiveBoundingBoxARB
          glGetTextureHandleARB
          glGetTextureSamplerHandleARB
          glMakeTextureHandleResidentARB
          glMakeTextureHandleNonResidentARB
          glGetImageHandleARB
          glMakeImageHandleResidentARB
          glMakeImageHandleNonResidentARB
          glUniformHandleui64ARB
          glUniformHandleui64vARB
          glProgramUniformHandleui64ARB
          glProgramUniformHandleui64vARB
          glIsTextureHandleResidentARB
          glIsImageHandleResidentARB
          glVertexAttribL1ui64ARB
          glVertexAttribL1ui64vARB
          glGetVertexAttribLui64vARB
          glCreateSyncFromCLeventARB
          glDispatchComputeGroupSizeARB
          glDebugMessageControlARB
          glDebugMessageInsertARB
          glDebugMessageCallbackARB
          glGetDebugMessageLogARB
          glBlendEquationiARB
          glBlendEquationSeparateiARB
          glBlendFunciARB
          glBlendFuncSeparateiARB
          glDrawArraysInstancedARB
          glDrawElementsInstancedARB
          glProgramParameteriARB
          glFramebufferTextureARB
          glFramebufferTextureLayerARB
          glFramebufferTextureFaceARB
          glSpecializeShaderARB
          glUniform1i64ARB
          glUniform2i64ARB
          glUniform3i64ARB
          glUniform4i64ARB
          glUniform1i64vARB
          glUniform2i64vARB
          glUniform3i64vARB
          glUniform4i64vARB
          glUniform1ui64ARB
          glUniform2ui64ARB
          glUniform3ui64ARB
          glUniform4ui64ARB
          glUniform1ui64vARB
          glUniform2ui64vARB
          glUniform3ui64vARB
          glUniform4ui64vARB
          glGetUniformi64vARB
          glGetUniformui64vARB
          glGetnUniformi64vARB
          glGetnUniformui64vARB
          glProgramUniform1i64ARB
          glProgramUniform2i64ARB
          glProgramUniform3i64ARB
          glProgramUniform4i64ARB
          glProgramUniform1i64vARB
          glProgramUniform2i64vARB
          glProgramUniform3i64vARB
          glProgramUniform4i64vARB
          glProgramUniform1ui64ARB
          glProgramUniform2ui64ARB
          glProgramUniform3ui64ARB
          glProgramUniform4ui64ARB
          glProgramUniform1ui64vARB
          glProgramUniform2ui64vARB
          glProgramUniform3ui64vARB
          glProgramUniform4ui64vARB
          glMultiDrawArraysIndirectCountARB
          glMultiDrawElementsIndirectCountARB
          glVertexAttribDivisorARB
          glMaxShaderCompilerThreadsARB
          glGetGraphicsResetStatusARB
          glGetnTexImageARB
          glReadnPixelsARB
          glGetnCompressedTexImageARB
          glGetnUniformfvARB
          glGetnUniformivARB
          glGetnUniformuivARB
          glGetnUniformdvARB
          glFramebufferSampleLocationsfvARB
          glNamedFramebufferSampleLocationsfvARB
          glEvaluateDepthValuesARB
          glMinSampleShadingARB
          glNamedStringARB
          glDeleteNamedStringARB
          glCompileShaderIncludeARB
          glIsNamedStringARB
          glGetNamedStringARB
          glGetNamedStringivARB
          glBufferPageCommitmentARB
          glNamedBufferPageCommitmentEXT
          glNamedBufferPageCommitmentARB
          glTexPageCommitmentARB
          glTexBufferARB
          glDepthRangeArraydvNV
          glDepthRangeIndexeddNV
          glBlendBarrierKHR
          glMaxShaderCompilerThreadsKHR
          glRenderbufferStorageMultisampleAdvancedAMD
          glNamedRenderbufferStorageMultisampleAdvancedAMD
          glGetPerfMonitorGroupsAMD
          glGetPerfMonitorCountersAMD
          glGetPerfMonitorGroupStringAMD
          glGetPerfMonitorCounterStringAMD
          glGetPerfMonitorCounterInfoAMD
          glGenPerfMonitorsAMD
          glDeletePerfMonitorsAMD
          glSelectPerfMonitorCountersAMD
          glBeginPerfMonitorAMD
          glEndPerfMonitorAMD
          glGetPerfMonitorCounterDataAMD
          glEGLImageTargetTexStorageEXT
          glEGLImageTargetTextureStorageEXT
          glLabelObjectEXT
          glGetObjectLabelEXT
          glInsertEventMarkerEXT
          glPushGroupMarkerEXT
          glPopGroupMarkerEXT
          glMatrixLoadfEXT
          glMatrixLoaddEXT
          glMatrixMultfEXT
          glMatrixMultdEXT
          glMatrixLoadIdentityEXT
          glMatrixRotatefEXT
          glMatrixRotatedEXT
          glMatrixScalefEXT
          glMatrixScaledEXT
          glMatrixTranslatefEXT
          glMatrixTranslatedEXT
          glMatrixFrustumEXT
          glMatrixOrthoEXT
          glMatrixPopEXT
          glMatrixPushEXT
          glClientAttribDefaultEXT
          glPushClientAttribDefaultEXT
          glTextureParameterfEXT
          glTextureParameterfvEXT
          glTextureParameteriEXT
          glTextureParameterivEXT
          glTextureImage1DEXT
          glTextureImage2DEXT
          glTextureSubImage1DEXT
          glTextureSubImage2DEXT
          glCopyTextureImage1DEXT
          glCopyTextureImage2DEXT
          glCopyTextureSubImage1DEXT
          glCopyTextureSubImage2DEXT
          glGetTextureImageEXT
          glGetTextureParameterfvEXT
          glGetTextureParameterivEXT
          glGetTextureLevelParameterfvEXT
          glGetTextureLevelParameterivEXT
          glTextureImage3DEXT
          glTextureSubImage3DEXT
          glCopyTextureSubImage3DEXT
          glBindMultiTextureEXT
          glMultiTexCoordPointerEXT
          glMultiTexEnvfEXT
          glMultiTexEnvfvEXT
          glMultiTexEnviEXT
          glMultiTexEnvivEXT
          glMultiTexGendEXT
          glMultiTexGendvEXT
          glMultiTexGenfEXT
          glMultiTexGenfvEXT
          glMultiTexGeniEXT
          glMultiTexGenivEXT
          glGetMultiTexEnvfvEXT
          glGetMultiTexEnvivEXT
          glGetMultiTexGendvEXT
          glGetMultiTexGenfvEXT
          glGetMultiTexGenivEXT
          glMultiTexParameteriEXT
          glMultiTexParameterivEXT
          glMultiTexParameterfEXT
          glMultiTexParameterfvEXT
          glMultiTexImage1DEXT
          glMultiTexImage2DEXT
          glMultiTexSubImage1DEXT
          glMultiTexSubImage2DEXT
          glCopyMultiTexImage1DEXT
          glCopyMultiTexImage2DEXT
          glCopyMultiTexSubImage1DEXT
          glCopyMultiTexSubImage2DEXT
          glGetMultiTexImageEXT
          glGetMultiTexParameterfvEXT
          glGetMultiTexParameterivEXT
          glGetMultiTexLevelParameterfvEXT
          glGetMultiTexLevelParameterivEXT
          glMultiTexImage3DEXT
          glMultiTexSubImage3DEXT
          glCopyMultiTexSubImage3DEXT
          glEnableClientStateIndexedEXT
          glDisableClientStateIndexedEXT
          glGetFloatIndexedvEXT
          glGetDoubleIndexedvEXT
          glGetPointerIndexedvEXT
          glEnableIndexedEXT
          glDisableIndexedEXT
          glIsEnabledIndexedEXT
          glGetIntegerIndexedvEXT
          glGetBooleanIndexedvEXT
          glCompressedTextureImage3DEXT
          glCompressedTextureImage2DEXT
          glCompressedTextureImage1DEXT
          glCompressedTextureSubImage3DEXT
          glCompressedTextureSubImage2DEXT
          glCompressedTextureSubImage1DEXT
          glGetCompressedTextureImageEXT
          glCompressedMultiTexImage3DEXT
          glCompressedMultiTexImage2DEXT
          glCompressedMultiTexImage1DEXT
          glCompressedMultiTexSubImage3DEXT
          glCompressedMultiTexSubImage2DEXT
          glCompressedMultiTexSubImage1DEXT
          glGetCompressedMultiTexImageEXT
          glMatrixLoadTransposefEXT
          glMatrixLoadTransposedEXT
          glMatrixMultTransposefEXT
          glMatrixMultTransposedEXT
          glNamedBufferDataEXT
          glNamedBufferSubDataEXT
          glUnmapNamedBufferEXT
          glGetNamedBufferParameterivEXT
          glGetNamedBufferPointervEXT
          glGetNamedBufferSubDataEXT
          glProgramUniform1fEXT
          glProgramUniform2fEXT
          glProgramUniform3fEXT
          glProgramUniform4fEXT
          glProgramUniform1iEXT
          glProgramUniform2iEXT
          glProgramUniform3iEXT
          glProgramUniform4iEXT
          glProgramUniform1fvEXT
          glProgramUniform2fvEXT
          glProgramUniform3fvEXT
          glProgramUniform4fvEXT
          glProgramUniform1ivEXT
          glProgramUniform2ivEXT
          glProgramUniform3ivEXT
          glProgramUniform4ivEXT
          glProgramUniformMatrix2fvEXT
          glProgramUniformMatrix3fvEXT
          glProgramUniformMatrix4fvEXT
          glProgramUniformMatrix2x3fvEXT
          glProgramUniformMatrix3x2fvEXT
          glProgramUniformMatrix2x4fvEXT
          glProgramUniformMatrix4x2fvEXT
          glProgramUniformMatrix3x4fvEXT
          glProgramUniformMatrix4x3fvEXT
          glTextureBufferEXT
          glMultiTexBufferEXT
          glTextureParameterIivEXT
          glTextureParameterIuivEXT
          glGetTextureParameterIivEXT
          glGetTextureParameterIuivEXT
          glMultiTexParameterIivEXT
          glMultiTexParameterIuivEXT
          glGetMultiTexParameterIivEXT
          glGetMultiTexParameterIuivEXT
          glProgramUniform1uiEXT
          glProgramUniform2uiEXT
          glProgramUniform3uiEXT
          glProgramUniform4uiEXT
          glProgramUniform1uivEXT
          glProgramUniform2uivEXT
          glProgramUniform3uivEXT
          glProgramUniform4uivEXT
          glNamedProgramLocalParameters4fvEXT
          glNamedProgramLocalParameterI4iEXT
          glNamedProgramLocalParameterI4ivEXT
          glNamedProgramLocalParametersI4ivEXT
          glNamedProgramLocalParameterI4uiEXT
          glNamedProgramLocalParameterI4uivEXT
          glNamedProgramLocalParametersI4uivEXT
          glGetNamedProgramLocalParameterIivEXT
          glGetNamedProgramLocalParameterIuivEXT
          glEnableClientStateiEXT
          glDisableClientStateiEXT
          glNamedProgramStringEXT
          glNamedProgramLocalParameter4dEXT
          glNamedProgramLocalParameter4dvEXT
          glNamedProgramLocalParameter4fEXT
          glNamedProgramLocalParameter4fvEXT
          glGetNamedProgramLocalParameterdvEXT
          glGetNamedProgramLocalParameterfvEXT
          glGetNamedProgramivEXT
          glGetNamedProgramStringEXT
          glNamedRenderbufferStorageEXT
          glGetNamedRenderbufferParameterivEXT
          glNamedRenderbufferStorageMultisampleEXT
          glNamedRenderbufferStorageMultisampleCoverageEXT
          glCheckNamedFramebufferStatusEXT
          glNamedFramebufferTexture1DEXT
          glNamedFramebufferTexture2DEXT
          glNamedFramebufferTexture3DEXT
          glNamedFramebufferRenderbufferEXT
          glGetNamedFramebufferAttachmentParameterivEXT
          glGenerateTextureMipmapEXT
          glGenerateMultiTexMipmapEXT
          glFramebufferDrawBufferEXT
          glFramebufferDrawBuffersEXT
          glFramebufferReadBufferEXT
          glGetFramebufferParameterivEXT
          glNamedCopyBufferSubDataEXT
          glNamedFramebufferTextureEXT
          glNamedFramebufferTextureLayerEXT
          glNamedFramebufferTextureFaceEXT
          glTextureRenderbufferEXT
          glMultiTexRenderbufferEXT
          glVertexArrayVertexOffsetEXT
          glVertexArrayColorOffsetEXT
          glVertexArrayEdgeFlagOffsetEXT
          glVertexArrayIndexOffsetEXT
          glVertexArrayNormalOffsetEXT
          glVertexArrayTexCoordOffsetEXT
          glVertexArrayMultiTexCoordOffsetEXT
          glVertexArrayFogCoordOffsetEXT
          glVertexArraySecondaryColorOffsetEXT
          glVertexArrayVertexAttribOffsetEXT
          glVertexArrayVertexAttribIOffsetEXT
          glEnableVertexArrayEXT
          glDisableVertexArrayEXT
          glEnableVertexArrayAttribEXT
          glDisableVertexArrayAttribEXT
          glGetVertexArrayIntegervEXT
          glGetVertexArrayPointervEXT
          glFlushMappedNamedBufferRangeEXT
          glNamedBufferStorageEXT
          glClearNamedBufferDataEXT
          glClearNamedBufferSubDataEXT
          glNamedFramebufferParameteriEXT
          glGetNamedFramebufferParameterivEXT
          glProgramUniform1dEXT
          glProgramUniform2dEXT
          glProgramUniform3dEXT
          glProgramUniform4dEXT
          glProgramUniform1dvEXT
          glProgramUniform2dvEXT
          glProgramUniform3dvEXT
          glProgramUniform4dvEXT
          glProgramUniformMatrix2dvEXT
          glProgramUniformMatrix3dvEXT
          glProgramUniformMatrix4dvEXT
          glProgramUniformMatrix2x3dvEXT
          glProgramUniformMatrix2x4dvEXT
          glProgramUniformMatrix3x2dvEXT
          glProgramUniformMatrix3x4dvEXT
          glProgramUniformMatrix4x2dvEXT
          glProgramUniformMatrix4x3dvEXT
          glTextureBufferRangeEXT
          glTextureStorage1DEXT
          glTextureStorage2DEXT
          glTextureStorage3DEXT
          glTextureStorage2DMultisampleEXT
          glTextureStorage3DMultisampleEXT
          glVertexArrayBindVertexBufferEXT
          glVertexArrayVertexAttribFormatEXT
          glVertexArrayVertexAttribIFormatEXT
          glVertexArrayVertexAttribLFormatEXT
          glVertexArrayVertexAttribBindingEXT
          glVertexArrayVertexBindingDivisorEXT
          glVertexArrayVertexAttribLOffsetEXT
          glTexturePageCommitmentEXT
          glVertexArrayVertexAttribDivisorEXT
          glDrawArraysInstancedEXT
          glDrawElementsInstancedEXT
          glPolygonOffsetClampEXT
          glRasterSamplesEXT
          glUseShaderProgramEXT
          glActiveProgramEXT
          glCreateShaderProgramEXT
          glFramebufferFetchBarrierEXT
          glWindowRectanglesEXT
          glApplyFramebufferAttachmentCMAAINTEL
          glBeginPerfQueryINTEL
          glCreatePerfQueryINTEL
          glDeletePerfQueryINTEL
          glEndPerfQueryINTEL
          glGetFirstPerfQueryIdINTEL
          glGetNextPerfQueryIdINTEL
          glGetPerfCounterInfoINTEL
          glGetPerfQueryDataINTEL
          glGetPerfQueryIdByNameINTEL
          glGetPerfQueryInfoINTEL
          glFramebufferParameteriMESA
          glGetFramebufferParameterivMESA
          glMultiDrawArraysIndirectBindlessNV
          glMultiDrawElementsIndirectBindlessNV
          glMultiDrawArraysIndirectBindlessCountNV
          glMultiDrawElementsIndirectBindlessCountNV
          glGetTextureHandleNV
          glGetTextureSamplerHandleNV
          glMakeTextureHandleResidentNV
          glMakeTextureHandleNonResidentNV
          glGetImageHandleNV
          glMakeImageHandleResidentNV
          glMakeImageHandleNonResidentNV
          glUniformHandleui64NV
          glUniformHandleui64vNV
          glProgramUniformHandleui64NV
          glProgramUniformHandleui64vNV
          glIsTextureHandleResidentNV
          glIsImageHandleResidentNV
          glBlendParameteriNV
          glBlendBarrierNV
          glViewportPositionWScaleNV
          glCreateStatesNV
          glDeleteStatesNV
          glIsStateNV
          glStateCaptureNV
          glGetCommandHeaderNV
          glGetStageIndexNV
          glDrawCommandsNV
          glDrawCommandsAddressNV
          glDrawCommandsStatesNV
          glDrawCommandsStatesAddressNV
          glCreateCommandListsNV
          glDeleteCommandListsNV
          glIsCommandListNV
          glListDrawCommandsStatesClientNV
          glCommandListSegmentsNV
          glCompileCommandListNV
          glCallCommandListNV
          glBeginConditionalRenderNV
          glEndConditionalRenderNV
          glSubpixelPrecisionBiasNV
          glConservativeRasterParameterfNV
          glConservativeRasterParameteriNV
          glDepthRangedNV
          glClearDepthdNV
          glDepthBoundsdNV
          glDrawVkImageNV
          glGetVkProcAddrNV
          glWaitVkSemaphoreNV
          glSignalVkSemaphoreNV
          glSignalVkFenceNV
          glFragmentCoverageColorNV
          glCoverageModulationTableNV
          glGetCoverageModulationTableNV
          glCoverageModulationNV
          glRenderbufferStorageMultisampleCoverageNV
          glUniform1i64NV
          glUniform2i64NV
          glUniform3i64NV
          glUniform4i64NV
          glUniform1i64vNV
          glUniform2i64vNV
          glUniform3i64vNV
          glUniform4i64vNV
          glUniform1ui64NV
          glUniform2ui64NV
          glUniform3ui64NV
          glUniform4ui64NV
          glUniform1ui64vNV
          glUniform2ui64vNV
          glUniform3ui64vNV
          glUniform4ui64vNV
          glGetUniformi64vNV
          glProgramUniform1i64NV
          glProgramUniform2i64NV
          glProgramUniform3i64NV
          glProgramUniform4i64NV
          glProgramUniform1i64vNV
          glProgramUniform2i64vNV
          glProgramUniform3i64vNV
          glProgramUniform4i64vNV
          glProgramUniform1ui64NV
          glProgramUniform2ui64NV
          glProgramUniform3ui64NV
          glProgramUniform4ui64NV
          glProgramUniform1ui64vNV
          glProgramUniform2ui64vNV
          glProgramUniform3ui64vNV
          glProgramUniform4ui64vNV
          glGetInternalformatSampleivNV
          glGetMemoryObjectDetachedResourcesuivNV
          glResetMemoryObjectParameterNV
          glTexAttachMemoryNV
          glBufferAttachMemoryNV
          glTextureAttachMemoryNV
          glNamedBufferAttachMemoryNV
          glDrawMeshTasksNV
          glDrawMeshTasksIndirectNV
          glMultiDrawMeshTasksIndirectNV
          glMultiDrawMeshTasksIndirectCountNV
          glGenPathsNV
          glDeletePathsNV
          glIsPathNV
          glPathCommandsNV
          glPathCoordsNV
          glPathSubCommandsNV
          glPathSubCoordsNV
          glPathStringNV
          glPathGlyphsNV
          glPathGlyphRangeNV
          glWeightPathsNV
          glCopyPathNV
          glInterpolatePathsNV
          glTransformPathNV
          glPathParameterivNV
          glPathParameteriNV
          glPathParameterfvNV
          glPathParameterfNV
          glPathDashArrayNV
          glPathStencilFuncNV
          glPathStencilDepthOffsetNV
          glStencilFillPathNV
          glStencilStrokePathNV
          glStencilFillPathInstancedNV
          glStencilStrokePathInstancedNV
          glPathCoverDepthFuncNV
          glCoverFillPathNV
          glCoverStrokePathNV
          glCoverFillPathInstancedNV
          glCoverStrokePathInstancedNV
          glGetPathParameterivNV
          glGetPathParameterfvNV
          glGetPathCommandsNV
          glGetPathCoordsNV
          glGetPathDashArrayNV
          glGetPathMetricsNV
          glGetPathMetricRangeNV
          glGetPathSpacingNV
          glIsPointInFillPathNV
          glIsPointInStrokePathNV
          glGetPathLengthNV
          glPointAlongPathNV
          glMatrixLoad3x2fNV
          glMatrixLoad3x3fNV
          glMatrixLoadTranspose3x3fNV
          glMatrixMult3x2fNV
          glMatrixMult3x3fNV
          glMatrixMultTranspose3x3fNV
          glStencilThenCoverFillPathNV
          glStencilThenCoverStrokePathNV
          glStencilThenCoverFillPathInstancedNV
          glStencilThenCoverStrokePathInstancedNV
          glPathGlyphIndexRangeNV
          glPathGlyphIndexArrayNV
          glPathMemoryGlyphIndexArrayNV
          glProgramPathFragmentInputGenNV
          glGetProgramResourcefvNV
          glFramebufferSampleLocationsfvNV
          glNamedFramebufferSampleLocationsfvNV
          glResolveDepthValuesNV
          glScissorExclusiveNV
          glScissorExclusiveArrayvNV
          glMakeBufferResidentNV
          glMakeBufferNonResidentNV
          glIsBufferResidentNV
          glMakeNamedBufferResidentNV
          glMakeNamedBufferNonResidentNV
          glIsNamedBufferResidentNV
          glGetBufferParameterui64vNV
          glGetNamedBufferParameterui64vNV
          glGetIntegerui64vNV
          glUniformui64NV
          glUniformui64vNV
          glGetUniformui64vNV
          glProgramUniformui64NV
          glProgramUniformui64vNV
          glBindShadingRateImageNV
          glGetShadingRateImagePaletteNV
          glGetShadingRateSampleLocationivNV
          glShadingRateImageBarrierNV
          glShadingRateImagePaletteNV
          glShadingRateSampleOrderNV
          glShadingRateSampleOrderCustomNV
          glTextureBarrierNV
          glVertexAttribL1i64NV
          glVertexAttribL2i64NV
          glVertexAttribL3i64NV
          glVertexAttribL4i64NV
          glVertexAttribL1i64vNV
          glVertexAttribL2i64vNV
          glVertexAttribL3i64vNV
          glVertexAttribL4i64vNV
          glVertexAttribL1ui64NV
          glVertexAttribL2ui64NV
          glVertexAttribL3ui64NV
          glVertexAttribL4ui64NV
          glVertexAttribL1ui64vNV
          glVertexAttribL2ui64vNV
          glVertexAttribL3ui64vNV
          glVertexAttribL4ui64vNV
          glGetVertexAttribLi64vNV
          glGetVertexAttribLui64vNV
          glVertexAttribLFormatNV
          glBufferAddressRangeNV
          glVertexFormatNV
          glNormalFormatNV
          glColorFormatNV
          glIndexFormatNV
          glTexCoordFormatNV
          glEdgeFlagFormatNV
          glSecondaryColorFormatNV
          glFogCoordFormatNV
          glVertexAttribFormatNV
          glVertexAttribIFormatNV
          glViewportSwizzleNV
          glFramebufferTextureMultiviewOVR
          GL_VERSION_1_0
          GL_DEPTH_BUFFER_BIT
          GL_STENCIL_BUFFER_BIT
          GL_COLOR_BUFFER_BIT
          GL_FALSE
          GL_TRUE
          GL_POINTS
          GL_LINES
          GL_LINE_LOOP
          GL_LINE_STRIP
          GL_TRIANGLES
          GL_TRIANGLE_STRIP
          GL_TRIANGLE_FAN
          GL_QUADS
          GL_NEVER
          GL_LESS
          GL_EQUAL
          GL_LEQUAL
          GL_GREATER
          GL_NOTEQUAL
          GL_GEQUAL
          GL_ALWAYS
          GL_ZERO
          GL_ONE
          GL_SRC_COLOR
          GL_ONE_MINUS_SRC_COLOR
          GL_SRC_ALPHA
          GL_ONE_MINUS_SRC_ALPHA
          GL_DST_ALPHA
          GL_ONE_MINUS_DST_ALPHA
          GL_DST_COLOR
          GL_ONE_MINUS_DST_COLOR
          GL_SRC_ALPHA_SATURATE
          GL_NONE
          GL_FRONT_LEFT
          GL_FRONT_RIGHT
          GL_BACK_LEFT
          GL_BACK_RIGHT
          GL_FRONT
          GL_BACK
          GL_LEFT
          GL_RIGHT
          GL_FRONT_AND_BACK
          GL_NO_ERROR
          GL_INVALID_ENUM
          GL_INVALID_VALUE
          GL_INVALID_OPERATION
          GL_OUT_OF_MEMORY
          GL_CW
          GL_CCW
          GL_POINT_SIZE
          GL_POINT_SIZE_RANGE
          GL_POINT_SIZE_GRANULARITY
          GL_LINE_SMOOTH
          GL_LINE_WIDTH
          GL_LINE_WIDTH_RANGE
          GL_LINE_WIDTH_GRANULARITY
          GL_POLYGON_MODE
          GL_POLYGON_SMOOTH
          GL_CULL_FACE
          GL_CULL_FACE_MODE
          GL_FRONT_FACE
          GL_DEPTH_RANGE
          GL_DEPTH_TEST
          GL_DEPTH_WRITEMASK
          GL_DEPTH_CLEAR_VALUE
          GL_DEPTH_FUNC
          GL_STENCIL_TEST
          GL_STENCIL_CLEAR_VALUE
          GL_STENCIL_FUNC
          GL_STENCIL_VALUE_MASK
          GL_STENCIL_FAIL
          GL_STENCIL_PASS_DEPTH_FAIL
          GL_STENCIL_PASS_DEPTH_PASS
          GL_STENCIL_REF
          GL_STENCIL_WRITEMASK
          GL_VIEWPORT
          GL_DITHER
          GL_BLEND_DST
          GL_BLEND_SRC
          GL_BLEND
          GL_LOGIC_OP_MODE
          GL_DRAW_BUFFER
          GL_READ_BUFFER
          GL_SCISSOR_BOX
          GL_SCISSOR_TEST
          GL_COLOR_CLEAR_VALUE
          GL_COLOR_WRITEMASK
          GL_DOUBLEBUFFER
          GL_STEREO
          GL_LINE_SMOOTH_HINT
          GL_POLYGON_SMOOTH_HINT
          GL_UNPACK_SWAP_BYTES
          GL_UNPACK_LSB_FIRST
          GL_UNPACK_ROW_LENGTH
          GL_UNPACK_SKIP_ROWS
          GL_UNPACK_SKIP_PIXELS
          GL_UNPACK_ALIGNMENT
          GL_PACK_SWAP_BYTES
          GL_PACK_LSB_FIRST
          GL_PACK_ROW_LENGTH
          GL_PACK_SKIP_ROWS
          GL_PACK_SKIP_PIXELS
          GL_PACK_ALIGNMENT
          GL_MAX_TEXTURE_SIZE
          GL_MAX_VIEWPORT_DIMS
          GL_SUBPIXEL_BITS
          GL_TEXTURE_1D
          GL_TEXTURE_2D
          GL_TEXTURE_WIDTH
          GL_TEXTURE_HEIGHT
          GL_TEXTURE_BORDER_COLOR
          GL_DONT_CARE
          GL_FASTEST
          GL_NICEST
          GL_BYTE
          GL_UNSIGNED_BYTE
          GL_SHORT
          GL_UNSIGNED_SHORT
          GL_INT
          GL_UNSIGNED_INT
          GL_FLOAT
          GL_STACK_OVERFLOW
          GL_STACK_UNDERFLOW
          GL_CLEAR
          GL_AND
          GL_AND_REVERSE
          GL_COPY
          GL_AND_INVERTED
          GL_NOOP
          GL_XOR
          GL_OR
          GL_NOR
          GL_EQUIV
          GL_INVERT
          GL_OR_REVERSE
          GL_COPY_INVERTED
          GL_OR_INVERTED
          GL_NAND
          GL_SET
          GL_TEXTURE
          GL_COLOR
          GL_DEPTH
          GL_STENCIL
          GL_STENCIL_INDEX
          GL_DEPTH_COMPONENT
          GL_RED
          GL_GREEN
          GL_BLUE
          GL_ALPHA
          GL_RGB
          GL_RGBA
          GL_POINT
          GL_LINE
          GL_FILL
          GL_KEEP
          GL_REPLACE
          GL_INCR
          GL_DECR
          GL_VENDOR
          GL_RENDERER
          GL_VERSION
          GL_EXTENSIONS
          GL_NEAREST
          GL_LINEAR
          GL_NEAREST_MIPMAP_NEAREST
          GL_LINEAR_MIPMAP_NEAREST
          GL_NEAREST_MIPMAP_LINEAR
          GL_LINEAR_MIPMAP_LINEAR
          GL_TEXTURE_MAG_FILTER
          GL_TEXTURE_MIN_FILTER
          GL_TEXTURE_WRAP_S
          GL_TEXTURE_WRAP_T
          GL_REPEAT
          GL_VERSION_1_1
          GL_COLOR_LOGIC_OP
          GL_POLYGON_OFFSET_UNITS
          GL_POLYGON_OFFSET_POINT
          GL_POLYGON_OFFSET_LINE
          GL_POLYGON_OFFSET_FILL
          GL_POLYGON_OFFSET_FACTOR
          GL_TEXTURE_BINDING_1D
          GL_TEXTURE_BINDING_2D
          GL_TEXTURE_INTERNAL_FORMAT
          GL_TEXTURE_RED_SIZE
          GL_TEXTURE_GREEN_SIZE
          GL_TEXTURE_BLUE_SIZE
          GL_TEXTURE_ALPHA_SIZE
          GL_DOUBLE
          GL_PROXY_TEXTURE_1D
          GL_PROXY_TEXTURE_2D
          GL_R3_G3_B2
          GL_RGB4
          GL_RGB5
          GL_RGB8
          GL_RGB10
          GL_RGB12
          GL_RGB16
          GL_RGBA2
          GL_RGBA4
          GL_RGB5_A1
          GL_RGBA8
          GL_RGB10_A2
          GL_RGBA12
          GL_RGBA16
          GL_VERTEX_ARRAY
          GL_VERSION_1_2
          GL_UNSIGNED_BYTE_3_3_2
          GL_UNSIGNED_SHORT_4_4_4_4
          GL_UNSIGNED_SHORT_5_5_5_1
          GL_UNSIGNED_INT_8_8_8_8
          GL_UNSIGNED_INT_10_10_10_2
          GL_TEXTURE_BINDING_3D
          GL_PACK_SKIP_IMAGES
          GL_PACK_IMAGE_HEIGHT
          GL_UNPACK_SKIP_IMAGES
          GL_UNPACK_IMAGE_HEIGHT
          GL_TEXTURE_3D
          GL_PROXY_TEXTURE_3D
          GL_TEXTURE_DEPTH
          GL_TEXTURE_WRAP_R
          GL_MAX_3D_TEXTURE_SIZE
          GL_UNSIGNED_BYTE_2_3_3_REV
          GL_UNSIGNED_SHORT_5_6_5
          GL_UNSIGNED_SHORT_5_6_5_REV
          GL_UNSIGNED_SHORT_4_4_4_4_REV
          GL_UNSIGNED_SHORT_1_5_5_5_REV
          GL_UNSIGNED_INT_8_8_8_8_REV
          GL_UNSIGNED_INT_2_10_10_10_REV
          GL_BGR
          GL_BGRA
          GL_MAX_ELEMENTS_VERTICES
          GL_MAX_ELEMENTS_INDICES
          GL_CLAMP_TO_EDGE
          GL_TEXTURE_MIN_LOD
          GL_TEXTURE_MAX_LOD
          GL_TEXTURE_BASE_LEVEL
          GL_TEXTURE_MAX_LEVEL
          GL_SMOOTH_POINT_SIZE_RANGE
          GL_SMOOTH_POINT_SIZE_GRANULARITY
          GL_SMOOTH_LINE_WIDTH_RANGE
          GL_SMOOTH_LINE_WIDTH_GRANULARITY
          GL_ALIASED_LINE_WIDTH_RANGE
          GL_VERSION_1_3
          GL_TEXTURE0
          GL_TEXTURE1
          GL_TEXTURE2
          GL_TEXTURE3
          GL_TEXTURE4
          GL_TEXTURE5
          GL_TEXTURE6
          GL_TEXTURE7
          GL_TEXTURE8
          GL_TEXTURE9
          GL_TEXTURE10
          GL_TEXTURE11
          GL_TEXTURE12
          GL_TEXTURE13
          GL_TEXTURE14
          GL_TEXTURE15
          GL_TEXTURE16
          GL_TEXTURE17
          GL_TEXTURE18
          GL_TEXTURE19
          GL_TEXTURE20
          GL_TEXTURE21
          GL_TEXTURE22
          GL_TEXTURE23
          GL_TEXTURE24
          GL_TEXTURE25
          GL_TEXTURE26
          GL_TEXTURE27
          GL_TEXTURE28
          GL_TEXTURE29
          GL_TEXTURE30
          GL_TEXTURE31
          GL_ACTIVE_TEXTURE
          GL_MULTISAMPLE
          GL_SAMPLE_ALPHA_TO_COVERAGE
          GL_SAMPLE_ALPHA_TO_ONE
          GL_SAMPLE_COVERAGE
          GL_SAMPLE_BUFFERS
          GL_SAMPLES
          GL_SAMPLE_COVERAGE_VALUE
          GL_SAMPLE_COVERAGE_INVERT
          GL_TEXTURE_CUBE_MAP
          GL_TEXTURE_BINDING_CUBE_MAP
          GL_TEXTURE_CUBE_MAP_POSITIVE_X
          GL_TEXTURE_CUBE_MAP_NEGATIVE_X
          GL_TEXTURE_CUBE_MAP_POSITIVE_Y
          GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
          GL_TEXTURE_CUBE_MAP_POSITIVE_Z
          GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
          GL_PROXY_TEXTURE_CUBE_MAP
          GL_MAX_CUBE_MAP_TEXTURE_SIZE
          GL_COMPRESSED_RGB
          GL_COMPRESSED_RGBA
          GL_TEXTURE_COMPRESSION_HINT
          GL_TEXTURE_COMPRESSED_IMAGE_SIZE
          GL_TEXTURE_COMPRESSED
          GL_NUM_COMPRESSED_TEXTURE_FORMATS
          GL_COMPRESSED_TEXTURE_FORMATS
          GL_CLAMP_TO_BORDER
          GL_VERSION_1_4
          GL_BLEND_DST_RGB
          GL_BLEND_SRC_RGB
          GL_BLEND_DST_ALPHA
          GL_BLEND_SRC_ALPHA
          GL_POINT_FADE_THRESHOLD_SIZE
          GL_DEPTH_COMPONENT16
          GL_DEPTH_COMPONENT24
          GL_DEPTH_COMPONENT32
          GL_MIRRORED_REPEAT
          GL_MAX_TEXTURE_LOD_BIAS
          GL_TEXTURE_LOD_BIAS
          GL_INCR_WRAP
          GL_DECR_WRAP
          GL_TEXTURE_DEPTH_SIZE
          GL_TEXTURE_COMPARE_MODE
          GL_TEXTURE_COMPARE_FUNC
          GL_BLEND_COLOR
          GL_BLEND_EQUATION
          GL_CONSTANT_COLOR
          GL_ONE_MINUS_CONSTANT_COLOR
          GL_CONSTANT_ALPHA
          GL_ONE_MINUS_CONSTANT_ALPHA
          GL_FUNC_ADD
          GL_FUNC_REVERSE_SUBTRACT
          GL_FUNC_SUBTRACT
          GL_MIN
          GL_MAX
          GL_VERSION_1_5
          GL_BUFFER_SIZE
          GL_BUFFER_USAGE
          GL_QUERY_COUNTER_BITS
          GL_CURRENT_QUERY
          GL_QUERY_RESULT
          GL_QUERY_RESULT_AVAILABLE
          GL_ARRAY_BUFFER
          GL_ELEMENT_ARRAY_BUFFER
          GL_ARRAY_BUFFER_BINDING
          GL_ELEMENT_ARRAY_BUFFER_BINDING
          GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
          GL_READ_ONLY
          GL_WRITE_ONLY
          GL_READ_WRITE
          GL_BUFFER_ACCESS
          GL_BUFFER_MAPPED
          GL_BUFFER_MAP_POINTER
          GL_STREAM_DRAW
          GL_STREAM_READ
          GL_STREAM_COPY
          GL_STATIC_DRAW
          GL_STATIC_READ
          GL_STATIC_COPY
          GL_DYNAMIC_DRAW
          GL_DYNAMIC_READ
          GL_DYNAMIC_COPY
          GL_SAMPLES_PASSED
          GL_SRC1_ALPHA
          GL_VERSION_2_0
          GL_BLEND_EQUATION_RGB
          GL_VERTEX_ATTRIB_ARRAY_ENABLED
          GL_VERTEX_ATTRIB_ARRAY_SIZE
          GL_VERTEX_ATTRIB_ARRAY_STRIDE
          GL_VERTEX_ATTRIB_ARRAY_TYPE
          GL_CURRENT_VERTEX_ATTRIB
          GL_VERTEX_PROGRAM_POINT_SIZE
          GL_VERTEX_ATTRIB_ARRAY_POINTER
          GL_STENCIL_BACK_FUNC
          GL_STENCIL_BACK_FAIL
          GL_STENCIL_BACK_PASS_DEPTH_FAIL
          GL_STENCIL_BACK_PASS_DEPTH_PASS
          GL_MAX_DRAW_BUFFERS
          GL_DRAW_BUFFER0
          GL_DRAW_BUFFER1
          GL_DRAW_BUFFER2
          GL_DRAW_BUFFER3
          GL_DRAW_BUFFER4
          GL_DRAW_BUFFER5
          GL_DRAW_BUFFER6
          GL_DRAW_BUFFER7
          GL_DRAW_BUFFER8
          GL_DRAW_BUFFER9
          GL_DRAW_BUFFER10
          GL_DRAW_BUFFER11
          GL_DRAW_BUFFER12
          GL_DRAW_BUFFER13
          GL_DRAW_BUFFER14
          GL_DRAW_BUFFER15
          GL_BLEND_EQUATION_ALPHA
          GL_MAX_VERTEX_ATTRIBS
          GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
          GL_MAX_TEXTURE_IMAGE_UNITS
          GL_FRAGMENT_SHADER
          GL_VERTEX_SHADER
          GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
          GL_MAX_VERTEX_UNIFORM_COMPONENTS
          GL_MAX_VARYING_FLOATS
          GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
          GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
          GL_SHADER_TYPE
          GL_FLOAT_VEC2
          GL_FLOAT_VEC3
          GL_FLOAT_VEC4
          GL_INT_VEC2
          GL_INT_VEC3
          GL_INT_VEC4
          GL_BOOL
          GL_BOOL_VEC2
          GL_BOOL_VEC3
          GL_BOOL_VEC4
          GL_FLOAT_MAT2
          GL_FLOAT_MAT3
          GL_FLOAT_MAT4
          GL_SAMPLER_1D
          GL_SAMPLER_2D
          GL_SAMPLER_3D
          GL_SAMPLER_CUBE
          GL_SAMPLER_1D_SHADOW
          GL_SAMPLER_2D_SHADOW
          GL_DELETE_STATUS
          GL_COMPILE_STATUS
          GL_LINK_STATUS
          GL_VALIDATE_STATUS
          GL_INFO_LOG_LENGTH
          GL_ATTACHED_SHADERS
          GL_ACTIVE_UNIFORMS
          GL_ACTIVE_UNIFORM_MAX_LENGTH
          GL_SHADER_SOURCE_LENGTH
          GL_ACTIVE_ATTRIBUTES
          GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
          GL_FRAGMENT_SHADER_DERIVATIVE_HINT
          GL_SHADING_LANGUAGE_VERSION
          GL_CURRENT_PROGRAM
          GL_POINT_SPRITE_COORD_ORIGIN
          GL_LOWER_LEFT
          GL_UPPER_LEFT
          GL_STENCIL_BACK_REF
          GL_STENCIL_BACK_VALUE_MASK
          GL_STENCIL_BACK_WRITEMASK
          GL_VERSION_2_1
          GL_PIXEL_PACK_BUFFER
          GL_PIXEL_UNPACK_BUFFER
          GL_PIXEL_PACK_BUFFER_BINDING
          GL_PIXEL_UNPACK_BUFFER_BINDING
          GL_SRGB
          GL_SRGB8
          GL_SRGB_ALPHA
          GL_SRGB8_ALPHA8
          GL_COMPRESSED_SRGB
          GL_COMPRESSED_SRGB_ALPHA
          GL_VERSION_3_0
          GL_COMPARE_REF_TO_TEXTURE
          GL_CLIP_DISTANCE0
          GL_CLIP_DISTANCE1
          GL_CLIP_DISTANCE2
          GL_CLIP_DISTANCE3
          GL_CLIP_DISTANCE4
          GL_CLIP_DISTANCE5
          GL_CLIP_DISTANCE6
          GL_CLIP_DISTANCE7
          GL_MAX_CLIP_DISTANCES
          GL_MAJOR_VERSION
          GL_MINOR_VERSION
          GL_NUM_EXTENSIONS
          GL_CONTEXT_FLAGS
          GL_COMPRESSED_RED
          GL_COMPRESSED_RG
          GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT
          GL_RGBA32F
          GL_RGB32F
          GL_RGBA16F
          GL_RGB16F
          GL_VERTEX_ATTRIB_ARRAY_INTEGER
          GL_MAX_ARRAY_TEXTURE_LAYERS
          GL_MIN_PROGRAM_TEXEL_OFFSET
          GL_MAX_PROGRAM_TEXEL_OFFSET
          GL_CLAMP_READ_COLOR
          GL_FIXED_ONLY
          GL_MAX_VARYING_COMPONENTS
          GL_TEXTURE_1D_ARRAY
          GL_PROXY_TEXTURE_1D_ARRAY
          GL_TEXTURE_2D_ARRAY
          GL_PROXY_TEXTURE_2D_ARRAY
          GL_TEXTURE_BINDING_1D_ARRAY
          GL_TEXTURE_BINDING_2D_ARRAY
          GL_R11F_G11F_B10F
          GL_UNSIGNED_INT_10F_11F_11F_REV
          GL_RGB9_E5
          GL_UNSIGNED_INT_5_9_9_9_REV
          GL_TEXTURE_SHARED_SIZE
          GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
          GL_TRANSFORM_FEEDBACK_BUFFER_MODE
          GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
          GL_TRANSFORM_FEEDBACK_VARYINGS
          GL_TRANSFORM_FEEDBACK_BUFFER_START
          GL_TRANSFORM_FEEDBACK_BUFFER_SIZE
          GL_PRIMITIVES_GENERATED
          GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
          GL_RASTERIZER_DISCARD
          GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
          GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
          GL_INTERLEAVED_ATTRIBS
          GL_SEPARATE_ATTRIBS
          GL_TRANSFORM_FEEDBACK_BUFFER
          GL_TRANSFORM_FEEDBACK_BUFFER_BINDING
          GL_RGBA32UI
          GL_RGB32UI
          GL_RGBA16UI
          GL_RGB16UI
          GL_RGBA8UI
          GL_RGB8UI
          GL_RGBA32I
          GL_RGB32I
          GL_RGBA16I
          GL_RGB16I
          GL_RGBA8I
          GL_RGB8I
          GL_RED_INTEGER
          GL_GREEN_INTEGER
          GL_BLUE_INTEGER
          GL_RGB_INTEGER
          GL_RGBA_INTEGER
          GL_BGR_INTEGER
          GL_BGRA_INTEGER
          GL_SAMPLER_1D_ARRAY
          GL_SAMPLER_2D_ARRAY
          GL_SAMPLER_1D_ARRAY_SHADOW
          GL_SAMPLER_2D_ARRAY_SHADOW
          GL_SAMPLER_CUBE_SHADOW
          GL_UNSIGNED_INT_VEC2
          GL_UNSIGNED_INT_VEC3
          GL_UNSIGNED_INT_VEC4
          GL_INT_SAMPLER_1D
          GL_INT_SAMPLER_2D
          GL_INT_SAMPLER_3D
          GL_INT_SAMPLER_CUBE
          GL_INT_SAMPLER_1D_ARRAY
          GL_INT_SAMPLER_2D_ARRAY
          GL_UNSIGNED_INT_SAMPLER_1D
          GL_UNSIGNED_INT_SAMPLER_2D
          GL_UNSIGNED_INT_SAMPLER_3D
          GL_UNSIGNED_INT_SAMPLER_CUBE
          GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
          GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
          GL_QUERY_WAIT
          GL_QUERY_NO_WAIT
          GL_QUERY_BY_REGION_WAIT
          GL_QUERY_BY_REGION_NO_WAIT
          GL_BUFFER_ACCESS_FLAGS
          GL_BUFFER_MAP_LENGTH
          GL_BUFFER_MAP_OFFSET
          GL_DEPTH_COMPONENT32F
          GL_DEPTH32F_STENCIL8
          GL_FLOAT_32_UNSIGNED_INT_24_8_REV
          GL_INVALID_FRAMEBUFFER_OPERATION
          GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING
          GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE
          GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE
          GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
          GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
          GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
          GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
          GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE
          GL_FRAMEBUFFER_DEFAULT
          GL_FRAMEBUFFER_UNDEFINED
          GL_DEPTH_STENCIL_ATTACHMENT
          GL_MAX_RENDERBUFFER_SIZE
          GL_DEPTH_STENCIL
          GL_UNSIGNED_INT_24_8
          GL_DEPTH24_STENCIL8
          GL_TEXTURE_STENCIL_SIZE
          GL_TEXTURE_RED_TYPE
          GL_TEXTURE_GREEN_TYPE
          GL_TEXTURE_BLUE_TYPE
          GL_TEXTURE_ALPHA_TYPE
          GL_TEXTURE_DEPTH_TYPE
          GL_UNSIGNED_NORMALIZED
          GL_FRAMEBUFFER_BINDING
          GL_DRAW_FRAMEBUFFER_BINDING
          GL_RENDERBUFFER_BINDING
          GL_READ_FRAMEBUFFER
          GL_DRAW_FRAMEBUFFER
          GL_READ_FRAMEBUFFER_BINDING
          GL_RENDERBUFFER_SAMPLES
          GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
          GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
          GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
          GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
          GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
          GL_FRAMEBUFFER_COMPLETE
          GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
          GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
          GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER
          GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER
          GL_FRAMEBUFFER_UNSUPPORTED
          GL_MAX_COLOR_ATTACHMENTS
          GL_COLOR_ATTACHMENT0
          GL_COLOR_ATTACHMENT1
          GL_COLOR_ATTACHMENT2
          GL_COLOR_ATTACHMENT3
          GL_COLOR_ATTACHMENT4
          GL_COLOR_ATTACHMENT5
          GL_COLOR_ATTACHMENT6
          GL_COLOR_ATTACHMENT7
          GL_COLOR_ATTACHMENT8
          GL_COLOR_ATTACHMENT9
          GL_COLOR_ATTACHMENT10
          GL_COLOR_ATTACHMENT11
          GL_COLOR_ATTACHMENT12
          GL_COLOR_ATTACHMENT13
          GL_COLOR_ATTACHMENT14
          GL_COLOR_ATTACHMENT15
          GL_COLOR_ATTACHMENT16
          GL_COLOR_ATTACHMENT17
          GL_COLOR_ATTACHMENT18
          GL_COLOR_ATTACHMENT19
          GL_COLOR_ATTACHMENT20
          GL_COLOR_ATTACHMENT21
          GL_COLOR_ATTACHMENT22
          GL_COLOR_ATTACHMENT23
          GL_COLOR_ATTACHMENT24
          GL_COLOR_ATTACHMENT25
          GL_COLOR_ATTACHMENT26
          GL_COLOR_ATTACHMENT27
          GL_COLOR_ATTACHMENT28
          GL_COLOR_ATTACHMENT29
          GL_COLOR_ATTACHMENT30
          GL_COLOR_ATTACHMENT31
          GL_DEPTH_ATTACHMENT
          GL_STENCIL_ATTACHMENT
          GL_FRAMEBUFFER
          GL_RENDERBUFFER
          GL_RENDERBUFFER_WIDTH
          GL_RENDERBUFFER_HEIGHT
          GL_RENDERBUFFER_INTERNAL_FORMAT
          GL_STENCIL_INDEX1
          GL_STENCIL_INDEX4
          GL_STENCIL_INDEX8
          GL_STENCIL_INDEX16
          GL_RENDERBUFFER_RED_SIZE
          GL_RENDERBUFFER_GREEN_SIZE
          GL_RENDERBUFFER_BLUE_SIZE
          GL_RENDERBUFFER_ALPHA_SIZE
          GL_RENDERBUFFER_DEPTH_SIZE
          GL_RENDERBUFFER_STENCIL_SIZE
          GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
          GL_MAX_SAMPLES
          GL_FRAMEBUFFER_SRGB
          GL_HALF_FLOAT
          GL_MAP_READ_BIT
          GL_MAP_WRITE_BIT
          GL_MAP_INVALIDATE_RANGE_BIT
          GL_MAP_INVALIDATE_BUFFER_BIT
          GL_MAP_FLUSH_EXPLICIT_BIT
          GL_MAP_UNSYNCHRONIZED_BIT
          GL_COMPRESSED_RED_RGTC1
          GL_COMPRESSED_SIGNED_RED_RGTC1
          GL_COMPRESSED_RG_RGTC2
          GL_COMPRESSED_SIGNED_RG_RGTC2
          GL_RG
          GL_RG_INTEGER
          GL_R8
          GL_R16
          GL_RG8
          GL_RG16
          GL_R16F
          GL_R32F
          GL_RG16F
          GL_RG32F
          GL_R8I
          GL_R8UI
          GL_R16I
          GL_R16UI
          GL_R32I
          GL_R32UI
          GL_RG8I
          GL_RG8UI
          GL_RG16I
          GL_RG16UI
          GL_RG32I
          GL_RG32UI
          GL_VERTEX_ARRAY_BINDING
          GL_VERSION_3_1
          GL_SAMPLER_2D_RECT
          GL_SAMPLER_2D_RECT_SHADOW
          GL_SAMPLER_BUFFER
          GL_INT_SAMPLER_2D_RECT
          GL_INT_SAMPLER_BUFFER
          GL_UNSIGNED_INT_SAMPLER_2D_RECT
          GL_UNSIGNED_INT_SAMPLER_BUFFER
          GL_TEXTURE_BUFFER
          GL_MAX_TEXTURE_BUFFER_SIZE
          GL_TEXTURE_BINDING_BUFFER
          GL_TEXTURE_BUFFER_DATA_STORE_BINDING
          GL_TEXTURE_RECTANGLE
          GL_TEXTURE_BINDING_RECTANGLE
          GL_PROXY_TEXTURE_RECTANGLE
          GL_MAX_RECTANGLE_TEXTURE_SIZE
          GL_R8_SNORM
          GL_RG8_SNORM
          GL_RGB8_SNORM
          GL_RGBA8_SNORM
          GL_R16_SNORM
          GL_RG16_SNORM
          GL_RGB16_SNORM
          GL_RGBA16_SNORM
          GL_SIGNED_NORMALIZED
          GL_PRIMITIVE_RESTART
          GL_PRIMITIVE_RESTART_INDEX
          GL_COPY_READ_BUFFER
          GL_COPY_WRITE_BUFFER
          GL_UNIFORM_BUFFER
          GL_UNIFORM_BUFFER_BINDING
          GL_UNIFORM_BUFFER_START
          GL_UNIFORM_BUFFER_SIZE
          GL_MAX_VERTEX_UNIFORM_BLOCKS
          GL_MAX_GEOMETRY_UNIFORM_BLOCKS
          GL_MAX_FRAGMENT_UNIFORM_BLOCKS
          GL_MAX_COMBINED_UNIFORM_BLOCKS
          GL_MAX_UNIFORM_BUFFER_BINDINGS
          GL_MAX_UNIFORM_BLOCK_SIZE
          GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS
          GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS
          GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS
          GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT
          GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH
          GL_ACTIVE_UNIFORM_BLOCKS
          GL_UNIFORM_TYPE
          GL_UNIFORM_SIZE
          GL_UNIFORM_NAME_LENGTH
          GL_UNIFORM_BLOCK_INDEX
          GL_UNIFORM_OFFSET
          GL_UNIFORM_ARRAY_STRIDE
          GL_UNIFORM_MATRIX_STRIDE
          GL_UNIFORM_IS_ROW_MAJOR
          GL_UNIFORM_BLOCK_BINDING
          GL_UNIFORM_BLOCK_DATA_SIZE
          GL_UNIFORM_BLOCK_NAME_LENGTH
          GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS
          GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES
          GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER
          GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER
          GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER
          GL_INVALID_INDEX
          GL_VERSION_3_2
          GL_CONTEXT_CORE_PROFILE_BIT
          GL_CONTEXT_COMPATIBILITY_PROFILE_BIT
          GL_LINES_ADJACENCY
          GL_LINE_STRIP_ADJACENCY
          GL_TRIANGLES_ADJACENCY
          GL_TRIANGLE_STRIP_ADJACENCY
          GL_PROGRAM_POINT_SIZE
          GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
          GL_FRAMEBUFFER_ATTACHMENT_LAYERED
          GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
          GL_GEOMETRY_SHADER
          GL_GEOMETRY_VERTICES_OUT
          GL_GEOMETRY_INPUT_TYPE
          GL_GEOMETRY_OUTPUT_TYPE
          GL_MAX_GEOMETRY_UNIFORM_COMPONENTS
          GL_MAX_GEOMETRY_OUTPUT_VERTICES
          GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS
          GL_MAX_VERTEX_OUTPUT_COMPONENTS
          GL_MAX_GEOMETRY_INPUT_COMPONENTS
          GL_MAX_GEOMETRY_OUTPUT_COMPONENTS
          GL_MAX_FRAGMENT_INPUT_COMPONENTS
          GL_CONTEXT_PROFILE_MASK
          GL_DEPTH_CLAMP
          GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION
          GL_FIRST_VERTEX_CONVENTION
          GL_LAST_VERTEX_CONVENTION
          GL_PROVOKING_VERTEX
          GL_TEXTURE_CUBE_MAP_SEAMLESS
          GL_MAX_SERVER_WAIT_TIMEOUT
          GL_OBJECT_TYPE
          GL_SYNC_CONDITION
          GL_SYNC_STATUS
          GL_SYNC_FLAGS
          GL_SYNC_FENCE
          GL_SYNC_GPU_COMMANDS_COMPLETE
          GL_UNSIGNALED
          GL_SIGNALED
          GL_ALREADY_SIGNALED
          GL_TIMEOUT_EXPIRED
          GL_CONDITION_SATISFIED
          GL_WAIT_FAILED
          GL_TIMEOUT_IGNORED
          GL_SYNC_FLUSH_COMMANDS_BIT
          GL_SAMPLE_POSITION
          GL_SAMPLE_MASK
          GL_SAMPLE_MASK_VALUE
          GL_MAX_SAMPLE_MASK_WORDS
          GL_TEXTURE_2D_MULTISAMPLE
          GL_PROXY_TEXTURE_2D_MULTISAMPLE
          GL_TEXTURE_2D_MULTISAMPLE_ARRAY
          GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY
          GL_TEXTURE_BINDING_2D_MULTISAMPLE
          GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
          GL_TEXTURE_SAMPLES
          GL_TEXTURE_FIXED_SAMPLE_LOCATIONS
          GL_SAMPLER_2D_MULTISAMPLE
          GL_INT_SAMPLER_2D_MULTISAMPLE
          GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
          GL_SAMPLER_2D_MULTISAMPLE_ARRAY
          GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
          GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
          GL_MAX_COLOR_TEXTURE_SAMPLES
          GL_MAX_DEPTH_TEXTURE_SAMPLES
          GL_MAX_INTEGER_SAMPLES
          GL_VERSION_3_3
          GL_VERTEX_ATTRIB_ARRAY_DIVISOR
          GL_SRC1_COLOR
          GL_ONE_MINUS_SRC1_COLOR
          GL_ONE_MINUS_SRC1_ALPHA
          GL_MAX_DUAL_SOURCE_DRAW_BUFFERS
          GL_ANY_SAMPLES_PASSED
          GL_SAMPLER_BINDING
          GL_RGB10_A2UI
          GL_TEXTURE_SWIZZLE_R
          GL_TEXTURE_SWIZZLE_G
          GL_TEXTURE_SWIZZLE_B
          GL_TEXTURE_SWIZZLE_A
          GL_TEXTURE_SWIZZLE_RGBA
          GL_TIME_ELAPSED
          GL_TIMESTAMP
          GL_INT_2_10_10_10_REV
          GL_VERSION_4_0
          GL_SAMPLE_SHADING
          GL_MIN_SAMPLE_SHADING_VALUE
          GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET
          GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET
          GL_TEXTURE_CUBE_MAP_ARRAY
          GL_TEXTURE_BINDING_CUBE_MAP_ARRAY
          GL_PROXY_TEXTURE_CUBE_MAP_ARRAY
          GL_SAMPLER_CUBE_MAP_ARRAY
          GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW
          GL_INT_SAMPLER_CUBE_MAP_ARRAY
          GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY
          GL_DRAW_INDIRECT_BUFFER
          GL_DRAW_INDIRECT_BUFFER_BINDING
          GL_GEOMETRY_SHADER_INVOCATIONS
          GL_MAX_GEOMETRY_SHADER_INVOCATIONS
          GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
          GL_MAX_FRAGMENT_INTERPOLATION_OFFSET
          GL_FRAGMENT_INTERPOLATION_OFFSET_BITS
          GL_MAX_VERTEX_STREAMS
          GL_DOUBLE_VEC2
          GL_DOUBLE_VEC3
          GL_DOUBLE_VEC4
          GL_DOUBLE_MAT2
          GL_DOUBLE_MAT3
          GL_DOUBLE_MAT4
          GL_ACTIVE_SUBROUTINES
          GL_ACTIVE_SUBROUTINE_UNIFORMS
          GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS
          GL_ACTIVE_SUBROUTINE_MAX_LENGTH
          GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH
          GL_MAX_SUBROUTINES
          GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS
          GL_NUM_COMPATIBLE_SUBROUTINES
          GL_COMPATIBLE_SUBROUTINES
          GL_PATCHES
          GL_PATCH_VERTICES
          GL_PATCH_DEFAULT_INNER_LEVEL
          GL_PATCH_DEFAULT_OUTER_LEVEL
          GL_TESS_CONTROL_OUTPUT_VERTICES
          GL_TESS_GEN_MODE
          GL_TESS_GEN_SPACING
          GL_TESS_GEN_VERTEX_ORDER
          GL_TESS_GEN_POINT_MODE
          GL_ISOLINES
          GL_FRACTIONAL_ODD
          GL_FRACTIONAL_EVEN
          GL_MAX_PATCH_VERTICES
          GL_MAX_TESS_GEN_LEVEL
          GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS
          GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS
          GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS
          GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS
          GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS
          GL_MAX_TESS_PATCH_COMPONENTS
          GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS
          GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS
          GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS
          GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS
          GL_MAX_TESS_CONTROL_INPUT_COMPONENTS
          GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS
          GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS
          GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS
          GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER
          GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER
          GL_TESS_EVALUATION_SHADER
          GL_TESS_CONTROL_SHADER
          GL_TRANSFORM_FEEDBACK
          GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED
          GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE
          GL_TRANSFORM_FEEDBACK_BINDING
          GL_MAX_TRANSFORM_FEEDBACK_BUFFERS
          GL_VERSION_4_1
          GL_FIXED
          GL_IMPLEMENTATION_COLOR_READ_TYPE
          GL_IMPLEMENTATION_COLOR_READ_FORMAT
          GL_LOW_FLOAT
          GL_MEDIUM_FLOAT
          GL_HIGH_FLOAT
          GL_LOW_INT
          GL_MEDIUM_INT
          GL_HIGH_INT
          GL_SHADER_COMPILER
          GL_SHADER_BINARY_FORMATS
          GL_NUM_SHADER_BINARY_FORMATS
          GL_MAX_VERTEX_UNIFORM_VECTORS
          GL_MAX_VARYING_VECTORS
          GL_MAX_FRAGMENT_UNIFORM_VECTORS
          GL_RGB565
          GL_PROGRAM_BINARY_RETRIEVABLE_HINT
          GL_PROGRAM_BINARY_LENGTH
          GL_NUM_PROGRAM_BINARY_FORMATS
          GL_PROGRAM_BINARY_FORMATS
          GL_VERTEX_SHADER_BIT
          GL_FRAGMENT_SHADER_BIT
          GL_GEOMETRY_SHADER_BIT
          GL_TESS_CONTROL_SHADER_BIT
          GL_TESS_EVALUATION_SHADER_BIT
          GL_ALL_SHADER_BITS
          GL_PROGRAM_SEPARABLE
          GL_ACTIVE_PROGRAM
          GL_PROGRAM_PIPELINE_BINDING
          GL_MAX_VIEWPORTS
          GL_VIEWPORT_SUBPIXEL_BITS
          GL_VIEWPORT_BOUNDS_RANGE
          GL_LAYER_PROVOKING_VERTEX
          GL_VIEWPORT_INDEX_PROVOKING_VERTEX
          GL_UNDEFINED_VERTEX
          GL_VERSION_4_2
          GL_COPY_READ_BUFFER_BINDING
          GL_COPY_WRITE_BUFFER_BINDING
          GL_TRANSFORM_FEEDBACK_ACTIVE
          GL_TRANSFORM_FEEDBACK_PAUSED
          GL_UNPACK_COMPRESSED_BLOCK_WIDTH
          GL_UNPACK_COMPRESSED_BLOCK_HEIGHT
          GL_UNPACK_COMPRESSED_BLOCK_DEPTH
          GL_UNPACK_COMPRESSED_BLOCK_SIZE
          GL_PACK_COMPRESSED_BLOCK_WIDTH
          GL_PACK_COMPRESSED_BLOCK_HEIGHT
          GL_PACK_COMPRESSED_BLOCK_DEPTH
          GL_PACK_COMPRESSED_BLOCK_SIZE
          GL_NUM_SAMPLE_COUNTS
          GL_MIN_MAP_BUFFER_ALIGNMENT
          GL_ATOMIC_COUNTER_BUFFER
          GL_ATOMIC_COUNTER_BUFFER_BINDING
          GL_ATOMIC_COUNTER_BUFFER_START
          GL_ATOMIC_COUNTER_BUFFER_SIZE
          GL_ATOMIC_COUNTER_BUFFER_DATA_SIZE
          GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTERS
          GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTER_INDICES
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_VERTEX_SHADER
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_CONTROL_SHADER
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_EVALUATION_SHADER
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_GEOMETRY_SHADER
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_FRAGMENT_SHADER
          GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS
          GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS
          GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS
          GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS
          GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS
          GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS
          GL_MAX_VERTEX_ATOMIC_COUNTERS
          GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS
          GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS
          GL_MAX_GEOMETRY_ATOMIC_COUNTERS
          GL_MAX_FRAGMENT_ATOMIC_COUNTERS
          GL_MAX_COMBINED_ATOMIC_COUNTERS
          GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE
          GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS
          GL_ACTIVE_ATOMIC_COUNTER_BUFFERS
          GL_UNIFORM_ATOMIC_COUNTER_BUFFER_INDEX
          GL_UNSIGNED_INT_ATOMIC_COUNTER
          GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT
          GL_ELEMENT_ARRAY_BARRIER_BIT
          GL_UNIFORM_BARRIER_BIT
          GL_TEXTURE_FETCH_BARRIER_BIT
          GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
          GL_COMMAND_BARRIER_BIT
          GL_PIXEL_BUFFER_BARRIER_BIT
          GL_TEXTURE_UPDATE_BARRIER_BIT
          GL_BUFFER_UPDATE_BARRIER_BIT
          GL_FRAMEBUFFER_BARRIER_BIT
          GL_TRANSFORM_FEEDBACK_BARRIER_BIT
          GL_ATOMIC_COUNTER_BARRIER_BIT
          GL_ALL_BARRIER_BITS
          GL_MAX_IMAGE_UNITS
          GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS
          GL_IMAGE_BINDING_NAME
          GL_IMAGE_BINDING_LEVEL
          GL_IMAGE_BINDING_LAYERED
          GL_IMAGE_BINDING_LAYER
          GL_IMAGE_BINDING_ACCESS
          GL_IMAGE_1D
          GL_IMAGE_2D
          GL_IMAGE_3D
          GL_IMAGE_2D_RECT
          GL_IMAGE_CUBE
          GL_IMAGE_BUFFER
          GL_IMAGE_1D_ARRAY
          GL_IMAGE_2D_ARRAY
          GL_IMAGE_CUBE_MAP_ARRAY
          GL_IMAGE_2D_MULTISAMPLE
          GL_IMAGE_2D_MULTISAMPLE_ARRAY
          GL_INT_IMAGE_1D
          GL_INT_IMAGE_2D
          GL_INT_IMAGE_3D
          GL_INT_IMAGE_2D_RECT
          GL_INT_IMAGE_CUBE
          GL_INT_IMAGE_BUFFER
          GL_INT_IMAGE_1D_ARRAY
          GL_INT_IMAGE_2D_ARRAY
          GL_INT_IMAGE_CUBE_MAP_ARRAY
          GL_INT_IMAGE_2D_MULTISAMPLE
          GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
          GL_UNSIGNED_INT_IMAGE_1D
          GL_UNSIGNED_INT_IMAGE_2D
          GL_UNSIGNED_INT_IMAGE_3D
          GL_UNSIGNED_INT_IMAGE_2D_RECT
          GL_UNSIGNED_INT_IMAGE_CUBE
          GL_UNSIGNED_INT_IMAGE_BUFFER
          GL_UNSIGNED_INT_IMAGE_1D_ARRAY
          GL_UNSIGNED_INT_IMAGE_2D_ARRAY
          GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY
          GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE
          GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY
          GL_MAX_IMAGE_SAMPLES
          GL_IMAGE_BINDING_FORMAT
          GL_IMAGE_FORMAT_COMPATIBILITY_TYPE
          GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE
          GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS
          GL_MAX_VERTEX_IMAGE_UNIFORMS
          GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS
          GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS
          GL_MAX_GEOMETRY_IMAGE_UNIFORMS
          GL_MAX_FRAGMENT_IMAGE_UNIFORMS
          GL_MAX_COMBINED_IMAGE_UNIFORMS
          GL_COMPRESSED_RGBA_BPTC_UNORM
          GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM
          GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT
          GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT
          GL_TEXTURE_IMMUTABLE_FORMAT
          GL_VERSION_4_3
          GL_NUM_SHADING_LANGUAGE_VERSIONS
          GL_VERTEX_ATTRIB_ARRAY_LONG
          GL_COMPRESSED_RGB8_ETC2
          GL_COMPRESSED_SRGB8_ETC2
          GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
          GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
          GL_COMPRESSED_RGBA8_ETC2_EAC
          GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
          GL_COMPRESSED_R11_EAC
          GL_COMPRESSED_SIGNED_R11_EAC
          GL_COMPRESSED_RG11_EAC
          GL_COMPRESSED_SIGNED_RG11_EAC
          GL_PRIMITIVE_RESTART_FIXED_INDEX
          GL_ANY_SAMPLES_PASSED_CONSERVATIVE
          GL_MAX_ELEMENT_INDEX
          GL_COMPUTE_SHADER
          GL_MAX_COMPUTE_UNIFORM_BLOCKS
          GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS
          GL_MAX_COMPUTE_IMAGE_UNIFORMS
          GL_MAX_COMPUTE_SHARED_MEMORY_SIZE
          GL_MAX_COMPUTE_UNIFORM_COMPONENTS
          GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS
          GL_MAX_COMPUTE_ATOMIC_COUNTERS
          GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS
          GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS
          GL_MAX_COMPUTE_WORK_GROUP_COUNT
          GL_MAX_COMPUTE_WORK_GROUP_SIZE
          GL_COMPUTE_WORK_GROUP_SIZE
          GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_COMPUTE_SHADER
          GL_DISPATCH_INDIRECT_BUFFER
          GL_DISPATCH_INDIRECT_BUFFER_BINDING
          GL_COMPUTE_SHADER_BIT
          GL_DEBUG_OUTPUT_SYNCHRONOUS
          GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH
          GL_DEBUG_CALLBACK_FUNCTION
          GL_DEBUG_CALLBACK_USER_PARAM
          GL_DEBUG_SOURCE_API
          GL_DEBUG_SOURCE_WINDOW_SYSTEM
          GL_DEBUG_SOURCE_SHADER_COMPILER
          GL_DEBUG_SOURCE_THIRD_PARTY
          GL_DEBUG_SOURCE_APPLICATION
          GL_DEBUG_SOURCE_OTHER
          GL_DEBUG_TYPE_ERROR
          GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR
          GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR
          GL_DEBUG_TYPE_PORTABILITY
          GL_DEBUG_TYPE_PERFORMANCE
          GL_DEBUG_TYPE_OTHER
          GL_MAX_DEBUG_MESSAGE_LENGTH
          GL_MAX_DEBUG_LOGGED_MESSAGES
          GL_DEBUG_LOGGED_MESSAGES
          GL_DEBUG_SEVERITY_HIGH
          GL_DEBUG_SEVERITY_MEDIUM
          GL_DEBUG_SEVERITY_LOW
          GL_DEBUG_TYPE_MARKER
          GL_DEBUG_TYPE_PUSH_GROUP
          GL_DEBUG_TYPE_POP_GROUP
          GL_DEBUG_SEVERITY_NOTIFICATION
          GL_MAX_DEBUG_GROUP_STACK_DEPTH
          GL_DEBUG_GROUP_STACK_DEPTH
          GL_BUFFER
          GL_SHADER
          GL_PROGRAM
          GL_QUERY
          GL_PROGRAM_PIPELINE
          GL_SAMPLER
          GL_MAX_LABEL_LENGTH
          GL_DEBUG_OUTPUT
          GL_CONTEXT_FLAG_DEBUG_BIT
          GL_MAX_UNIFORM_LOCATIONS
          GL_FRAMEBUFFER_DEFAULT_WIDTH
          GL_FRAMEBUFFER_DEFAULT_HEIGHT
          GL_FRAMEBUFFER_DEFAULT_LAYERS
          GL_FRAMEBUFFER_DEFAULT_SAMPLES
          GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS
          GL_MAX_FRAMEBUFFER_WIDTH
          GL_MAX_FRAMEBUFFER_HEIGHT
          GL_MAX_FRAMEBUFFER_LAYERS
          GL_MAX_FRAMEBUFFER_SAMPLES
          GL_INTERNALFORMAT_SUPPORTED
          GL_INTERNALFORMAT_PREFERRED
          GL_INTERNALFORMAT_RED_SIZE
          GL_INTERNALFORMAT_GREEN_SIZE
          GL_INTERNALFORMAT_BLUE_SIZE
          GL_INTERNALFORMAT_ALPHA_SIZE
          GL_INTERNALFORMAT_DEPTH_SIZE
          GL_INTERNALFORMAT_STENCIL_SIZE
          GL_INTERNALFORMAT_SHARED_SIZE
          GL_INTERNALFORMAT_RED_TYPE
          GL_INTERNALFORMAT_GREEN_TYPE
          GL_INTERNALFORMAT_BLUE_TYPE
          GL_INTERNALFORMAT_ALPHA_TYPE
          GL_INTERNALFORMAT_DEPTH_TYPE
          GL_INTERNALFORMAT_STENCIL_TYPE
          GL_MAX_WIDTH
          GL_MAX_HEIGHT
          GL_MAX_DEPTH
          GL_MAX_LAYERS
          GL_MAX_COMBINED_DIMENSIONS
          GL_COLOR_COMPONENTS
          GL_DEPTH_COMPONENTS
          GL_STENCIL_COMPONENTS
          GL_COLOR_RENDERABLE
          GL_DEPTH_RENDERABLE
          GL_STENCIL_RENDERABLE
          GL_FRAMEBUFFER_RENDERABLE
          GL_FRAMEBUFFER_RENDERABLE_LAYERED
          GL_FRAMEBUFFER_BLEND
          GL_READ_PIXELS
          GL_READ_PIXELS_FORMAT
          GL_READ_PIXELS_TYPE
          GL_TEXTURE_IMAGE_FORMAT
          GL_TEXTURE_IMAGE_TYPE
          GL_GET_TEXTURE_IMAGE_FORMAT
          GL_GET_TEXTURE_IMAGE_TYPE
          GL_MIPMAP
          GL_MANUAL_GENERATE_MIPMAP
          GL_AUTO_GENERATE_MIPMAP
          GL_COLOR_ENCODING
          GL_SRGB_READ
          GL_SRGB_WRITE
          GL_FILTER
          GL_VERTEX_TEXTURE
          GL_TESS_CONTROL_TEXTURE
          GL_TESS_EVALUATION_TEXTURE
          GL_GEOMETRY_TEXTURE
          GL_FRAGMENT_TEXTURE
          GL_COMPUTE_TEXTURE
          GL_TEXTURE_SHADOW
          GL_TEXTURE_GATHER
          GL_TEXTURE_GATHER_SHADOW
          GL_SHADER_IMAGE_LOAD
          GL_SHADER_IMAGE_STORE
          GL_SHADER_IMAGE_ATOMIC
          GL_IMAGE_TEXEL_SIZE
          GL_IMAGE_COMPATIBILITY_CLASS
          GL_IMAGE_PIXEL_FORMAT
          GL_IMAGE_PIXEL_TYPE
          GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_TEST
          GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_TEST
          GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_WRITE
          GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_WRITE
          GL_TEXTURE_COMPRESSED_BLOCK_WIDTH
          GL_TEXTURE_COMPRESSED_BLOCK_HEIGHT
          GL_TEXTURE_COMPRESSED_BLOCK_SIZE
          GL_CLEAR_BUFFER
          GL_TEXTURE_VIEW
          GL_VIEW_COMPATIBILITY_CLASS
          GL_FULL_SUPPORT
          GL_CAVEAT_SUPPORT
          GL_IMAGE_CLASS_4_X_32
          GL_IMAGE_CLASS_2_X_32
          GL_IMAGE_CLASS_1_X_32
          GL_IMAGE_CLASS_4_X_16
          GL_IMAGE_CLASS_2_X_16
          GL_IMAGE_CLASS_1_X_16
          GL_IMAGE_CLASS_4_X_8
          GL_IMAGE_CLASS_2_X_8
          GL_IMAGE_CLASS_1_X_8
          GL_IMAGE_CLASS_11_11_10
          GL_IMAGE_CLASS_10_10_10_2
          GL_VIEW_CLASS_128_BITS
          GL_VIEW_CLASS_96_BITS
          GL_VIEW_CLASS_64_BITS
          GL_VIEW_CLASS_48_BITS
          GL_VIEW_CLASS_32_BITS
          GL_VIEW_CLASS_24_BITS
          GL_VIEW_CLASS_16_BITS
          GL_VIEW_CLASS_8_BITS
          GL_VIEW_CLASS_S3TC_DXT1_RGB
          GL_VIEW_CLASS_S3TC_DXT1_RGBA
          GL_VIEW_CLASS_S3TC_DXT3_RGBA
          GL_VIEW_CLASS_S3TC_DXT5_RGBA
          GL_VIEW_CLASS_RGTC1_RED
          GL_VIEW_CLASS_RGTC2_RG
          GL_VIEW_CLASS_BPTC_UNORM
          GL_VIEW_CLASS_BPTC_FLOAT
          GL_UNIFORM
          GL_UNIFORM_BLOCK
          GL_PROGRAM_INPUT
          GL_PROGRAM_OUTPUT
          GL_BUFFER_VARIABLE
          GL_SHADER_STORAGE_BLOCK
          GL_VERTEX_SUBROUTINE
          GL_TESS_CONTROL_SUBROUTINE
          GL_TESS_EVALUATION_SUBROUTINE
          GL_GEOMETRY_SUBROUTINE
          GL_FRAGMENT_SUBROUTINE
          GL_COMPUTE_SUBROUTINE
          GL_VERTEX_SUBROUTINE_UNIFORM
          GL_TESS_CONTROL_SUBROUTINE_UNIFORM
          GL_TESS_EVALUATION_SUBROUTINE_UNIFORM
          GL_GEOMETRY_SUBROUTINE_UNIFORM
          GL_FRAGMENT_SUBROUTINE_UNIFORM
          GL_COMPUTE_SUBROUTINE_UNIFORM
          GL_TRANSFORM_FEEDBACK_VARYING
          GL_ACTIVE_RESOURCES
          GL_MAX_NAME_LENGTH
          GL_MAX_NUM_ACTIVE_VARIABLES
          GL_MAX_NUM_COMPATIBLE_SUBROUTINES
          GL_NAME_LENGTH
          GL_TYPE
          GL_ARRAY_SIZE
          GL_OFFSET
          GL_BLOCK_INDEX
          GL_ARRAY_STRIDE
          GL_MATRIX_STRIDE
          GL_IS_ROW_MAJOR
          GL_ATOMIC_COUNTER_BUFFER_INDEX
          GL_BUFFER_BINDING
          GL_BUFFER_DATA_SIZE
          GL_NUM_ACTIVE_VARIABLES
          GL_ACTIVE_VARIABLES
          GL_REFERENCED_BY_VERTEX_SHADER
          GL_REFERENCED_BY_TESS_CONTROL_SHADER
          GL_REFERENCED_BY_TESS_EVALUATION_SHADER
          GL_REFERENCED_BY_GEOMETRY_SHADER
          GL_REFERENCED_BY_FRAGMENT_SHADER
          GL_REFERENCED_BY_COMPUTE_SHADER
          GL_TOP_LEVEL_ARRAY_SIZE
          GL_TOP_LEVEL_ARRAY_STRIDE
          GL_LOCATION
          GL_LOCATION_INDEX
          GL_IS_PER_PATCH
          GL_SHADER_STORAGE_BUFFER
          GL_SHADER_STORAGE_BUFFER_BINDING
          GL_SHADER_STORAGE_BUFFER_START
          GL_SHADER_STORAGE_BUFFER_SIZE
          GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS
          GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS
          GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS
          GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS
          GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS
          GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS
          GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS
          GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS
          GL_MAX_SHADER_STORAGE_BLOCK_SIZE
          GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT
          GL_SHADER_STORAGE_BARRIER_BIT
          GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES
          GL_DEPTH_STENCIL_TEXTURE_MODE
          GL_TEXTURE_BUFFER_OFFSET
          GL_TEXTURE_BUFFER_SIZE
          GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT
          GL_TEXTURE_VIEW_MIN_LEVEL
          GL_TEXTURE_VIEW_NUM_LEVELS
          GL_TEXTURE_VIEW_MIN_LAYER
          GL_TEXTURE_VIEW_NUM_LAYERS
          GL_TEXTURE_IMMUTABLE_LEVELS
          GL_VERTEX_ATTRIB_BINDING
          GL_VERTEX_ATTRIB_RELATIVE_OFFSET
          GL_VERTEX_BINDING_DIVISOR
          GL_VERTEX_BINDING_OFFSET
          GL_VERTEX_BINDING_STRIDE
          GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET
          GL_MAX_VERTEX_ATTRIB_BINDINGS
          GL_VERTEX_BINDING_BUFFER
          GL_VERSION_4_4
          GL_MAX_VERTEX_ATTRIB_STRIDE
          GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED
          GL_TEXTURE_BUFFER_BINDING
          GL_MAP_PERSISTENT_BIT
          GL_MAP_COHERENT_BIT
          GL_DYNAMIC_STORAGE_BIT
          GL_CLIENT_STORAGE_BIT
          GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT
          GL_BUFFER_IMMUTABLE_STORAGE
          GL_BUFFER_STORAGE_FLAGS
          GL_CLEAR_TEXTURE
          GL_LOCATION_COMPONENT
          GL_TRANSFORM_FEEDBACK_BUFFER_INDEX
          GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE
          GL_QUERY_BUFFER
          GL_QUERY_BUFFER_BARRIER_BIT
          GL_QUERY_BUFFER_BINDING
          GL_QUERY_RESULT_NO_WAIT
          GL_MIRROR_CLAMP_TO_EDGE
          GL_VERSION_4_5
          GL_CONTEXT_LOST
          GL_NEGATIVE_ONE_TO_ONE
          GL_ZERO_TO_ONE
          GL_CLIP_ORIGIN
          GL_CLIP_DEPTH_MODE
          GL_QUERY_WAIT_INVERTED
          GL_QUERY_NO_WAIT_INVERTED
          GL_QUERY_BY_REGION_WAIT_INVERTED
          GL_QUERY_BY_REGION_NO_WAIT_INVERTED
          GL_MAX_CULL_DISTANCES
          GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES
          GL_TEXTURE_TARGET
          GL_QUERY_TARGET
          GL_GUILTY_CONTEXT_RESET
          GL_INNOCENT_CONTEXT_RESET
          GL_UNKNOWN_CONTEXT_RESET
          GL_RESET_NOTIFICATION_STRATEGY
          GL_LOSE_CONTEXT_ON_RESET
          GL_NO_RESET_NOTIFICATION
          GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT
          GL_CONTEXT_RELEASE_BEHAVIOR
          GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH
          GL_VERSION_4_6
          GL_SHADER_BINARY_FORMAT_SPIR_V
          GL_SPIR_V_BINARY
          GL_PARAMETER_BUFFER
          GL_PARAMETER_BUFFER_BINDING
          GL_CONTEXT_FLAG_NO_ERROR_BIT
          GL_VERTICES_SUBMITTED
          GL_PRIMITIVES_SUBMITTED
          GL_VERTEX_SHADER_INVOCATIONS
          GL_TESS_CONTROL_SHADER_PATCHES
          GL_TESS_EVALUATION_SHADER_INVOCATIONS
          GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED
          GL_FRAGMENT_SHADER_INVOCATIONS
          GL_COMPUTE_SHADER_INVOCATIONS
          GL_CLIPPING_INPUT_PRIMITIVES
          GL_CLIPPING_OUTPUT_PRIMITIVES
          GL_POLYGON_OFFSET_CLAMP
          GL_SPIR_V_EXTENSIONS
          GL_NUM_SPIR_V_EXTENSIONS
          GL_TEXTURE_MAX_ANISOTROPY
          GL_MAX_TEXTURE_MAX_ANISOTROPY
          GL_TRANSFORM_FEEDBACK_OVERFLOW
          GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW
          GL_PRIMITIVE_BOUNDING_BOX_ARB
          GL_MULTISAMPLE_LINE_WIDTH_RANGE_ARB
          GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY_ARB
          GL_UNSIGNED_INT64_ARB
          GL_SYNC_CL_EVENT_ARB
          GL_SYNC_CL_EVENT_COMPLETE_ARB
          GL_MAX_COMPUTE_VARIABLE_GROUP_INVOCATIONS_ARB
          GL_MAX_COMPUTE_FIXED_GROUP_INVOCATIONS_ARB
          GL_MAX_COMPUTE_VARIABLE_GROUP_SIZE_ARB
          GL_MAX_COMPUTE_FIXED_GROUP_SIZE_ARB
          GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB
          GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB
          GL_DEBUG_CALLBACK_FUNCTION_ARB
          GL_DEBUG_CALLBACK_USER_PARAM_ARB
          GL_DEBUG_SOURCE_API_ARB
          GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB
          GL_DEBUG_SOURCE_SHADER_COMPILER_ARB
          GL_DEBUG_SOURCE_THIRD_PARTY_ARB
          GL_DEBUG_SOURCE_APPLICATION_ARB
          GL_DEBUG_SOURCE_OTHER_ARB
          GL_DEBUG_TYPE_ERROR_ARB
          GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB
          GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB
          GL_DEBUG_TYPE_PORTABILITY_ARB
          GL_DEBUG_TYPE_PERFORMANCE_ARB
          GL_DEBUG_TYPE_OTHER_ARB
          GL_MAX_DEBUG_MESSAGE_LENGTH_ARB
          GL_MAX_DEBUG_LOGGED_MESSAGES_ARB
          GL_DEBUG_LOGGED_MESSAGES_ARB
          GL_DEBUG_SEVERITY_HIGH_ARB
          GL_DEBUG_SEVERITY_MEDIUM_ARB
          GL_DEBUG_SEVERITY_LOW_ARB
          GL_LINES_ADJACENCY_ARB
          GL_LINE_STRIP_ADJACENCY_ARB
          GL_TRIANGLES_ADJACENCY_ARB
          GL_TRIANGLE_STRIP_ADJACENCY_ARB
          GL_PROGRAM_POINT_SIZE_ARB
          GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB
          GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB
          GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB
          GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB
          GL_GEOMETRY_SHADER_ARB
          GL_GEOMETRY_VERTICES_OUT_ARB
          GL_GEOMETRY_INPUT_TYPE_ARB
          GL_GEOMETRY_OUTPUT_TYPE_ARB
          GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB
          GL_MAX_VERTEX_VARYING_COMPONENTS_ARB
          GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB
          GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB
          GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB
          GL_SHADER_BINARY_FORMAT_SPIR_V_ARB
          GL_SPIR_V_BINARY_ARB
          GL_INT64_ARB
          GL_INT64_VEC2_ARB
          GL_INT64_VEC3_ARB
          GL_INT64_VEC4_ARB
          GL_UNSIGNED_INT64_VEC2_ARB
          GL_UNSIGNED_INT64_VEC3_ARB
          GL_UNSIGNED_INT64_VEC4_ARB
          GL_PARAMETER_BUFFER_ARB
          GL_PARAMETER_BUFFER_BINDING_ARB
          GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB
          GL_SRGB_DECODE_ARB
          GL_VIEW_CLASS_EAC_R11
          GL_VIEW_CLASS_EAC_RG11
          GL_VIEW_CLASS_ETC2_RGB
          GL_VIEW_CLASS_ETC2_RGBA
          GL_VIEW_CLASS_ETC2_EAC_RGBA
          GL_MAX_SHADER_COMPILER_THREADS_ARB
          GL_COMPLETION_STATUS_ARB
          GL_VERTICES_SUBMITTED_ARB
          GL_PRIMITIVES_SUBMITTED_ARB
          GL_VERTEX_SHADER_INVOCATIONS_ARB
          GL_TESS_CONTROL_SHADER_PATCHES_ARB
          GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB
          GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB
          GL_FRAGMENT_SHADER_INVOCATIONS_ARB
          GL_COMPUTE_SHADER_INVOCATIONS_ARB
          GL_CLIPPING_INPUT_PRIMITIVES_ARB
          GL_CLIPPING_OUTPUT_PRIMITIVES_ARB
          GL_PIXEL_PACK_BUFFER_ARB
          GL_PIXEL_UNPACK_BUFFER_ARB
          GL_PIXEL_PACK_BUFFER_BINDING_ARB
          GL_PIXEL_UNPACK_BUFFER_BINDING_ARB
          GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB
          GL_LOSE_CONTEXT_ON_RESET_ARB
          GL_GUILTY_CONTEXT_RESET_ARB
          GL_INNOCENT_CONTEXT_RESET_ARB
          GL_UNKNOWN_CONTEXT_RESET_ARB
          GL_RESET_NOTIFICATION_STRATEGY_ARB
          GL_NO_RESET_NOTIFICATION_ARB
          GL_SAMPLE_LOCATION_SUBPIXEL_BITS_ARB
          GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_ARB
          GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_ARB
          GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_ARB
          GL_SAMPLE_LOCATION_ARB
          GL_PROGRAMMABLE_SAMPLE_LOCATION_ARB
          GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_ARB
          GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_ARB
          GL_SAMPLE_SHADING_ARB
          GL_MIN_SAMPLE_SHADING_VALUE_ARB
          GL_SHADER_INCLUDE_ARB
          GL_NAMED_STRING_LENGTH_ARB
          GL_NAMED_STRING_TYPE_ARB
          GL_SPARSE_STORAGE_BIT_ARB
          GL_SPARSE_BUFFER_PAGE_SIZE_ARB
          GL_TEXTURE_SPARSE_ARB
          GL_VIRTUAL_PAGE_SIZE_INDEX_ARB
          GL_NUM_SPARSE_LEVELS_ARB
          GL_NUM_VIRTUAL_PAGE_SIZES_ARB
          GL_VIRTUAL_PAGE_SIZE_X_ARB
          GL_VIRTUAL_PAGE_SIZE_Y_ARB
          GL_VIRTUAL_PAGE_SIZE_Z_ARB
          GL_MAX_SPARSE_TEXTURE_SIZE_ARB
          GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB
          GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB
          GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_ARB
          GL_CLAMP_TO_BORDER_ARB
          GL_TEXTURE_BUFFER_ARB
          GL_MAX_TEXTURE_BUFFER_SIZE_ARB
          GL_TEXTURE_BINDING_BUFFER_ARB
          GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB
          GL_TEXTURE_BUFFER_FORMAT_ARB
          GL_COMPRESSED_RGBA_BPTC_UNORM_ARB
          GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB
          GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB
          GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB
          GL_TEXTURE_CUBE_MAP_ARRAY_ARB
          GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB
          GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB
          GL_SAMPLER_CUBE_MAP_ARRAY_ARB
          GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB
          GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB
          GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB
          GL_TEXTURE_REDUCTION_MODE_ARB
          GL_WEIGHTED_AVERAGE_ARB
          GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB
          GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB
          GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB
          GL_MIRRORED_REPEAT_ARB
          GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB
          GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB
          GL_MULTIPLY_KHR
          GL_SCREEN_KHR
          GL_OVERLAY_KHR
          GL_DARKEN_KHR
          GL_LIGHTEN_KHR
          GL_COLORDODGE_KHR
          GL_COLORBURN_KHR
          GL_HARDLIGHT_KHR
          GL_SOFTLIGHT_KHR
          GL_DIFFERENCE_KHR
          GL_EXCLUSION_KHR
          GL_HSL_HUE_KHR
          GL_HSL_SATURATION_KHR
          GL_HSL_COLOR_KHR
          GL_HSL_LUMINOSITY_KHR
          GL_BLEND_ADVANCED_COHERENT_KHR
          GL_CONTEXT_FLAG_NO_ERROR_BIT_KHR
          GL_MAX_SHADER_COMPILER_THREADS_KHR
          GL_COMPLETION_STATUS_KHR
          GL_CONTEXT_ROBUST_ACCESS
          GL_SUBGROUP_SIZE_KHR
          GL_SUBGROUP_SUPPORTED_STAGES_KHR
          GL_SUBGROUP_SUPPORTED_FEATURES_KHR
          GL_SUBGROUP_QUAD_ALL_STAGES_KHR
          GL_SUBGROUP_FEATURE_BASIC_BIT_KHR
          GL_SUBGROUP_FEATURE_VOTE_BIT_KHR
          GL_SUBGROUP_FEATURE_ARITHMETIC_BIT_KHR
          GL_SUBGROUP_FEATURE_BALLOT_BIT_KHR
          GL_SUBGROUP_FEATURE_SHUFFLE_BIT_KHR
          GL_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT_KHR
          GL_SUBGROUP_FEATURE_CLUSTERED_BIT_KHR
          GL_SUBGROUP_FEATURE_QUAD_BIT_KHR
          GL_RENDERBUFFER_STORAGE_SAMPLES_AMD
          GL_MAX_COLOR_FRAMEBUFFER_SAMPLES_AMD
          GL_MAX_COLOR_FRAMEBUFFER_STORAGE_SAMPLES_AMD
          GL_MAX_DEPTH_STENCIL_FRAMEBUFFER_SAMPLES_AMD
          GL_NUM_SUPPORTED_MULTISAMPLE_MODES_AMD
          GL_SUPPORTED_MULTISAMPLE_MODES_AMD
          GL_COUNTER_TYPE_AMD
          GL_COUNTER_RANGE_AMD
          GL_UNSIGNED_INT64_AMD
          GL_PERCENTAGE_AMD
          GL_PERFMON_RESULT_AVAILABLE_AMD
          GL_PERFMON_RESULT_SIZE_AMD
          GL_PERFMON_RESULT_AMD
          GL_RGB_422_APPLE
          GL_UNSIGNED_SHORT_8_8_APPLE
          GL_UNSIGNED_SHORT_8_8_REV_APPLE
          GL_RGB_RAW_422_APPLE
          GL_PROGRAM_PIPELINE_OBJECT_EXT
          GL_PROGRAM_OBJECT_EXT
          GL_SHADER_OBJECT_EXT
          GL_BUFFER_OBJECT_EXT
          GL_QUERY_OBJECT_EXT
          GL_VERTEX_ARRAY_OBJECT_EXT
          GL_PROGRAM_MATRIX_EXT
          GL_TRANSPOSE_PROGRAM_MATRIX_EXT
          GL_PROGRAM_MATRIX_STACK_DEPTH_EXT
          GL_POLYGON_OFFSET_CLAMP_EXT
          GL_RASTER_MULTISAMPLE_EXT
          GL_RASTER_SAMPLES_EXT
          GL_MAX_RASTER_SAMPLES_EXT
          GL_RASTER_FIXED_SAMPLE_LOCATIONS_EXT
          GL_MULTISAMPLE_RASTERIZATION_ALLOWED_EXT
          GL_EFFECTIVE_RASTER_SAMPLES_EXT
          GL_ACTIVE_PROGRAM_EXT
          GL_FRAGMENT_SHADER_DISCARDS_SAMPLES_EXT
          GL_COMPRESSED_RGB_S3TC_DXT1_EXT
          GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
          GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
          GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
          GL_TEXTURE_REDUCTION_MODE_EXT
          GL_WEIGHTED_AVERAGE_EXT
          GL_SR8_EXT
          GL_TEXTURE_SRGB_DECODE_EXT
          GL_DECODE_EXT
          GL_SKIP_DECODE_EXT
          GL_INCLUSIVE_EXT
          GL_EXCLUSIVE_EXT
          GL_WINDOW_RECTANGLE_EXT
          GL_WINDOW_RECTANGLE_MODE_EXT
          GL_MAX_WINDOW_RECTANGLES_EXT
          GL_NUM_WINDOW_RECTANGLES_EXT
          GL_BLACKHOLE_RENDER_INTEL
          GL_CONSERVATIVE_RASTERIZATION_INTEL
          GL_PERFQUERY_SINGLE_CONTEXT_INTEL
          GL_PERFQUERY_GLOBAL_CONTEXT_INTEL
          GL_PERFQUERY_WAIT_INTEL
          GL_PERFQUERY_FLUSH_INTEL
          GL_PERFQUERY_DONOT_FLUSH_INTEL
          GL_PERFQUERY_COUNTER_EVENT_INTEL
          GL_PERFQUERY_COUNTER_DURATION_NORM_INTEL
          GL_PERFQUERY_COUNTER_DURATION_RAW_INTEL
          GL_PERFQUERY_COUNTER_THROUGHPUT_INTEL
          GL_PERFQUERY_COUNTER_RAW_INTEL
          GL_PERFQUERY_COUNTER_TIMESTAMP_INTEL
          GL_PERFQUERY_COUNTER_DATA_UINT32_INTEL
          GL_PERFQUERY_COUNTER_DATA_UINT64_INTEL
          GL_PERFQUERY_COUNTER_DATA_FLOAT_INTEL
          GL_PERFQUERY_COUNTER_DATA_DOUBLE_INTEL
          GL_PERFQUERY_COUNTER_DATA_BOOL32_INTEL
          GL_PERFQUERY_QUERY_NAME_LENGTH_MAX_INTEL
          GL_PERFQUERY_COUNTER_NAME_LENGTH_MAX_INTEL
          GL_PERFQUERY_COUNTER_DESC_LENGTH_MAX_INTEL
          GL_PERFQUERY_GPA_EXTENDED_COUNTERS_INTEL
          GL_FRAMEBUFFER_FLIP_Y_MESA
          GL_BLEND_OVERLAP_NV
          GL_BLEND_PREMULTIPLIED_SRC_NV
          GL_BLUE_NV
          GL_COLORBURN_NV
          GL_COLORDODGE_NV
          GL_CONJOINT_NV
          GL_CONTRAST_NV
          GL_DARKEN_NV
          GL_DIFFERENCE_NV
          GL_DISJOINT_NV
          GL_DST_ATOP_NV
          GL_DST_IN_NV
          GL_DST_NV
          GL_DST_OUT_NV
          GL_DST_OVER_NV
          GL_EXCLUSION_NV
          GL_GREEN_NV
          GL_HARDLIGHT_NV
          GL_HARDMIX_NV
          GL_HSL_COLOR_NV
          GL_HSL_HUE_NV
          GL_HSL_LUMINOSITY_NV
          GL_HSL_SATURATION_NV
          GL_INVERT_OVG_NV
          GL_INVERT_RGB_NV
          GL_LIGHTEN_NV
          GL_LINEARBURN_NV
          GL_LINEARDODGE_NV
          GL_LINEARLIGHT_NV
          GL_MINUS_CLAMPED_NV
          GL_MINUS_NV
          GL_MULTIPLY_NV
          GL_OVERLAY_NV
          GL_PINLIGHT_NV
          GL_PLUS_CLAMPED_ALPHA_NV
          GL_PLUS_CLAMPED_NV
          GL_PLUS_DARKER_NV
          GL_PLUS_NV
          GL_RED_NV
          GL_SCREEN_NV
          GL_SOFTLIGHT_NV
          GL_SRC_ATOP_NV
          GL_SRC_IN_NV
          GL_SRC_NV
          GL_SRC_OUT_NV
          GL_SRC_OVER_NV
          GL_UNCORRELATED_NV
          GL_VIVIDLIGHT_NV
          GL_XOR_NV
          GL_BLEND_ADVANCED_COHERENT_NV
          GL_FACTOR_MIN_AMD
          GL_FACTOR_MAX_AMD
          GL_VIEWPORT_POSITION_W_SCALE_NV
          GL_VIEWPORT_POSITION_W_SCALE_X_COEFF_NV
          GL_VIEWPORT_POSITION_W_SCALE_Y_COEFF_NV
          GL_TERMINATE_SEQUENCE_COMMAND_NV
          GL_NOP_COMMAND_NV
          GL_DRAW_ELEMENTS_COMMAND_NV
          GL_DRAW_ARRAYS_COMMAND_NV
          GL_DRAW_ELEMENTS_STRIP_COMMAND_NV
          GL_DRAW_ARRAYS_STRIP_COMMAND_NV
          GL_DRAW_ELEMENTS_INSTANCED_COMMAND_NV
          GL_DRAW_ARRAYS_INSTANCED_COMMAND_NV
          GL_ELEMENT_ADDRESS_COMMAND_NV
          GL_ATTRIBUTE_ADDRESS_COMMAND_NV
          GL_UNIFORM_ADDRESS_COMMAND_NV
          GL_BLEND_COLOR_COMMAND_NV
          GL_STENCIL_REF_COMMAND_NV
          GL_LINE_WIDTH_COMMAND_NV
          GL_POLYGON_OFFSET_COMMAND_NV
          GL_ALPHA_REF_COMMAND_NV
          GL_VIEWPORT_COMMAND_NV
          GL_SCISSOR_COMMAND_NV
          GL_FRONT_FACE_COMMAND_NV
          GL_QUERY_WAIT_NV
          GL_QUERY_NO_WAIT_NV
          GL_QUERY_BY_REGION_WAIT_NV
          GL_QUERY_BY_REGION_NO_WAIT_NV
          GL_CONSERVATIVE_RASTERIZATION_NV
          GL_SUBPIXEL_PRECISION_BIAS_X_BITS_NV
          GL_SUBPIXEL_PRECISION_BIAS_Y_BITS_NV
          GL_MAX_SUBPIXEL_PRECISION_BIAS_BITS_NV
          GL_CONSERVATIVE_RASTER_DILATE_NV
          GL_CONSERVATIVE_RASTER_DILATE_RANGE_NV
          GL_CONSERVATIVE_RASTER_DILATE_GRANULARITY_NV
          GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_NV
          GL_CONSERVATIVE_RASTER_MODE_NV
          GL_CONSERVATIVE_RASTER_MODE_POST_SNAP_NV
          GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_TRIANGLES_NV
          GL_DEPTH_COMPONENT32F_NV
          GL_DEPTH32F_STENCIL8_NV
          GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV
          GL_DEPTH_BUFFER_FLOAT_MODE_NV
          GL_FILL_RECTANGLE_NV
          GL_FRAGMENT_COVERAGE_TO_COLOR_NV
          GL_FRAGMENT_COVERAGE_COLOR_NV
          GL_COVERAGE_MODULATION_TABLE_NV
          GL_COLOR_SAMPLES_NV
          GL_DEPTH_SAMPLES_NV
          GL_STENCIL_SAMPLES_NV
          GL_MIXED_DEPTH_SAMPLES_SUPPORTED_NV
          GL_MIXED_STENCIL_SAMPLES_SUPPORTED_NV
          GL_COVERAGE_MODULATION_NV
          GL_COVERAGE_MODULATION_TABLE_SIZE_NV
          GL_RENDERBUFFER_COVERAGE_SAMPLES_NV
          GL_RENDERBUFFER_COLOR_SAMPLES_NV
          GL_MAX_MULTISAMPLE_COVERAGE_MODES_NV
          GL_MULTISAMPLE_COVERAGE_MODES_NV
          GL_INT64_NV
          GL_UNSIGNED_INT64_NV
          GL_INT8_NV
          GL_INT8_VEC2_NV
          GL_INT8_VEC3_NV
          GL_INT8_VEC4_NV
          GL_INT16_NV
          GL_INT16_VEC2_NV
          GL_INT16_VEC3_NV
          GL_INT16_VEC4_NV
          GL_INT64_VEC2_NV
          GL_INT64_VEC3_NV
          GL_INT64_VEC4_NV
          GL_UNSIGNED_INT8_NV
          GL_UNSIGNED_INT8_VEC2_NV
          GL_UNSIGNED_INT8_VEC3_NV
          GL_UNSIGNED_INT8_VEC4_NV
          GL_UNSIGNED_INT16_NV
          GL_UNSIGNED_INT16_VEC2_NV
          GL_UNSIGNED_INT16_VEC3_NV
          GL_UNSIGNED_INT16_VEC4_NV
          GL_UNSIGNED_INT64_VEC2_NV
          GL_UNSIGNED_INT64_VEC3_NV
          GL_UNSIGNED_INT64_VEC4_NV
          GL_FLOAT16_NV
          GL_FLOAT16_VEC2_NV
          GL_FLOAT16_VEC3_NV
          GL_FLOAT16_VEC4_NV
          GL_MULTISAMPLES_NV
          GL_SUPERSAMPLE_SCALE_X_NV
          GL_SUPERSAMPLE_SCALE_Y_NV
          GL_CONFORMANT_NV
          GL_ATTACHED_MEMORY_OBJECT_NV
          GL_ATTACHED_MEMORY_OFFSET_NV
          GL_MEMORY_ATTACHABLE_ALIGNMENT_NV
          GL_MEMORY_ATTACHABLE_SIZE_NV
          GL_MEMORY_ATTACHABLE_NV
          GL_DETACHED_MEMORY_INCARNATION_NV
          GL_DETACHED_TEXTURES_NV
          GL_DETACHED_BUFFERS_NV
          GL_MAX_DETACHED_TEXTURES_NV
          GL_MAX_DETACHED_BUFFERS_NV
          GL_MESH_SHADER_NV
          GL_TASK_SHADER_NV
          GL_MAX_MESH_UNIFORM_BLOCKS_NV
          GL_MAX_MESH_TEXTURE_IMAGE_UNITS_NV
          GL_MAX_MESH_IMAGE_UNIFORMS_NV
          GL_MAX_MESH_UNIFORM_COMPONENTS_NV
          GL_MAX_MESH_ATOMIC_COUNTER_BUFFERS_NV
          GL_MAX_MESH_ATOMIC_COUNTERS_NV
          GL_MAX_MESH_SHADER_STORAGE_BLOCKS_NV
          GL_MAX_COMBINED_MESH_UNIFORM_COMPONENTS_NV
          GL_MAX_TASK_UNIFORM_BLOCKS_NV
          GL_MAX_TASK_TEXTURE_IMAGE_UNITS_NV
          GL_MAX_TASK_IMAGE_UNIFORMS_NV
          GL_MAX_TASK_UNIFORM_COMPONENTS_NV
          GL_MAX_TASK_ATOMIC_COUNTER_BUFFERS_NV
          GL_MAX_TASK_ATOMIC_COUNTERS_NV
          GL_MAX_TASK_SHADER_STORAGE_BLOCKS_NV
          GL_MAX_COMBINED_TASK_UNIFORM_COMPONENTS_NV
          GL_MAX_MESH_WORK_GROUP_INVOCATIONS_NV
          GL_MAX_TASK_WORK_GROUP_INVOCATIONS_NV
          GL_MAX_MESH_TOTAL_MEMORY_SIZE_NV
          GL_MAX_TASK_TOTAL_MEMORY_SIZE_NV
          GL_MAX_MESH_OUTPUT_VERTICES_NV
          GL_MAX_MESH_OUTPUT_PRIMITIVES_NV
          GL_MAX_TASK_OUTPUT_COUNT_NV
          GL_MAX_DRAW_MESH_TASKS_COUNT_NV
          GL_MAX_MESH_VIEWS_NV
          GL_MESH_OUTPUT_PER_VERTEX_GRANULARITY_NV
          GL_MESH_OUTPUT_PER_PRIMITIVE_GRANULARITY_NV
          GL_MAX_MESH_WORK_GROUP_SIZE_NV
          GL_MAX_TASK_WORK_GROUP_SIZE_NV
          GL_MESH_WORK_GROUP_SIZE_NV
          GL_TASK_WORK_GROUP_SIZE_NV
          GL_MESH_VERTICES_OUT_NV
          GL_MESH_PRIMITIVES_OUT_NV
          GL_MESH_OUTPUT_TYPE_NV
          GL_UNIFORM_BLOCK_REFERENCED_BY_MESH_SHADER_NV
          GL_UNIFORM_BLOCK_REFERENCED_BY_TASK_SHADER_NV
          GL_REFERENCED_BY_MESH_SHADER_NV
          GL_REFERENCED_BY_TASK_SHADER_NV
          GL_MESH_SHADER_BIT_NV
          GL_TASK_SHADER_BIT_NV
          GL_MESH_SUBROUTINE_NV
          GL_TASK_SUBROUTINE_NV
          GL_MESH_SUBROUTINE_UNIFORM_NV
          GL_TASK_SUBROUTINE_UNIFORM_NV
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_MESH_SHADER_NV
          GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TASK_SHADER_NV
          GL_PATH_FORMAT_SVG_NV
          GL_PATH_FORMAT_PS_NV
          GL_STANDARD_FONT_NAME_NV
          GL_SYSTEM_FONT_NAME_NV
          GL_FILE_NAME_NV
          GL_PATH_STROKE_WIDTH_NV
          GL_PATH_END_CAPS_NV
          GL_PATH_INITIAL_END_CAP_NV
          GL_PATH_TERMINAL_END_CAP_NV
          GL_PATH_JOIN_STYLE_NV
          GL_PATH_MITER_LIMIT_NV
          GL_PATH_DASH_CAPS_NV
          GL_PATH_INITIAL_DASH_CAP_NV
          GL_PATH_TERMINAL_DASH_CAP_NV
          GL_PATH_DASH_OFFSET_NV
          GL_PATH_CLIENT_LENGTH_NV
          GL_PATH_FILL_MODE_NV
          GL_PATH_FILL_MASK_NV
          GL_PATH_FILL_COVER_MODE_NV
          GL_PATH_STROKE_COVER_MODE_NV
          GL_PATH_STROKE_MASK_NV
          GL_COUNT_UP_NV
          GL_COUNT_DOWN_NV
          GL_PATH_OBJECT_BOUNDING_BOX_NV
          GL_CONVEX_HULL_NV
          GL_BOUNDING_BOX_NV
          GL_TRANSLATE_X_NV
          GL_TRANSLATE_Y_NV
          GL_TRANSLATE_2D_NV
          GL_TRANSLATE_3D_NV
          GL_AFFINE_2D_NV
          GL_AFFINE_3D_NV
          GL_TRANSPOSE_AFFINE_2D_NV
          GL_TRANSPOSE_AFFINE_3D_NV
          GL_UTF8_NV
          GL_UTF16_NV
          GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV
          GL_PATH_COMMAND_COUNT_NV
          GL_PATH_COORD_COUNT_NV
          GL_PATH_DASH_ARRAY_COUNT_NV
          GL_PATH_COMPUTED_LENGTH_NV
          GL_PATH_FILL_BOUNDING_BOX_NV
          GL_PATH_STROKE_BOUNDING_BOX_NV
          GL_SQUARE_NV
          GL_ROUND_NV
          GL_TRIANGULAR_NV
          GL_BEVEL_NV
          GL_MITER_REVERT_NV
          GL_MITER_TRUNCATE_NV
          GL_SKIP_MISSING_GLYPH_NV
          GL_USE_MISSING_GLYPH_NV
          GL_PATH_ERROR_POSITION_NV
          GL_ACCUM_ADJACENT_PAIRS_NV
          GL_ADJACENT_PAIRS_NV
          GL_FIRST_TO_REST_NV
          GL_PATH_GEN_MODE_NV
          GL_PATH_GEN_COEFF_NV
          GL_PATH_GEN_COMPONENTS_NV
          GL_PATH_STENCIL_FUNC_NV
          GL_PATH_STENCIL_REF_NV
          GL_PATH_STENCIL_VALUE_MASK_NV
          GL_PATH_STENCIL_DEPTH_OFFSET_FACTOR_NV
          GL_PATH_STENCIL_DEPTH_OFFSET_UNITS_NV
          GL_PATH_COVER_DEPTH_FUNC_NV
          GL_PATH_DASH_OFFSET_RESET_NV
          GL_MOVE_TO_RESETS_NV
          GL_MOVE_TO_CONTINUES_NV
          GL_CLOSE_PATH_NV
          GL_MOVE_TO_NV
          GL_RELATIVE_MOVE_TO_NV
          GL_LINE_TO_NV
          GL_RELATIVE_LINE_TO_NV
          GL_HORIZONTAL_LINE_TO_NV
          GL_RELATIVE_HORIZONTAL_LINE_TO_NV
          GL_VERTICAL_LINE_TO_NV
          GL_RELATIVE_VERTICAL_LINE_TO_NV
          GL_QUADRATIC_CURVE_TO_NV
          GL_RELATIVE_QUADRATIC_CURVE_TO_NV
          GL_CUBIC_CURVE_TO_NV
          GL_RELATIVE_CUBIC_CURVE_TO_NV
          GL_SMOOTH_QUADRATIC_CURVE_TO_NV
          GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV
          GL_SMOOTH_CUBIC_CURVE_TO_NV
          GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV
          GL_SMALL_CCW_ARC_TO_NV
          GL_RELATIVE_SMALL_CCW_ARC_TO_NV
          GL_SMALL_CW_ARC_TO_NV
          GL_RELATIVE_SMALL_CW_ARC_TO_NV
          GL_LARGE_CCW_ARC_TO_NV
          GL_RELATIVE_LARGE_CCW_ARC_TO_NV
          GL_LARGE_CW_ARC_TO_NV
          GL_RELATIVE_LARGE_CW_ARC_TO_NV
          GL_RESTART_PATH_NV
          GL_DUP_FIRST_CUBIC_CURVE_TO_NV
          GL_DUP_LAST_CUBIC_CURVE_TO_NV
          GL_RECT_NV
          GL_CIRCULAR_CCW_ARC_TO_NV
          GL_CIRCULAR_CW_ARC_TO_NV
          GL_CIRCULAR_TANGENT_ARC_TO_NV
          GL_ARC_TO_NV
          GL_RELATIVE_ARC_TO_NV
          GL_BOLD_BIT_NV
          GL_ITALIC_BIT_NV
          GL_GLYPH_WIDTH_BIT_NV
          GL_GLYPH_HEIGHT_BIT_NV
          GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV
          GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV
          GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV
          GL_GLYPH_VERTICAL_BEARING_X_BIT_NV
          GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV
          GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV
          GL_GLYPH_HAS_KERNING_BIT_NV
          GL_FONT_X_MIN_BOUNDS_BIT_NV
          GL_FONT_Y_MIN_BOUNDS_BIT_NV
          GL_FONT_X_MAX_BOUNDS_BIT_NV
          GL_FONT_Y_MAX_BOUNDS_BIT_NV
          GL_FONT_UNITS_PER_EM_BIT_NV
          GL_FONT_ASCENDER_BIT_NV
          GL_FONT_DESCENDER_BIT_NV
          GL_FONT_HEIGHT_BIT_NV
          GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV
          GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV
          GL_FONT_UNDERLINE_POSITION_BIT_NV
          GL_FONT_UNDERLINE_THICKNESS_BIT_NV
          GL_FONT_HAS_KERNING_BIT_NV
          GL_ROUNDED_RECT_NV
          GL_RELATIVE_ROUNDED_RECT_NV
          GL_ROUNDED_RECT2_NV
          GL_RELATIVE_ROUNDED_RECT2_NV
          GL_ROUNDED_RECT4_NV
          GL_RELATIVE_ROUNDED_RECT4_NV
          GL_ROUNDED_RECT8_NV
          GL_RELATIVE_ROUNDED_RECT8_NV
          GL_RELATIVE_RECT_NV
          GL_FONT_GLYPHS_AVAILABLE_NV
          GL_FONT_TARGET_UNAVAILABLE_NV
          GL_FONT_UNAVAILABLE_NV
          GL_FONT_UNINTELLIGIBLE_NV
          GL_CONIC_CURVE_TO_NV
          GL_RELATIVE_CONIC_CURVE_TO_NV
          GL_FONT_NUM_GLYPH_INDICES_BIT_NV
          GL_STANDARD_FONT_FORMAT_NV
          GL_PATH_PROJECTION_NV
          GL_PATH_MODELVIEW_NV
          GL_PATH_MODELVIEW_STACK_DEPTH_NV
          GL_PATH_MODELVIEW_MATRIX_NV
          GL_PATH_MAX_MODELVIEW_STACK_DEPTH_NV
          GL_PATH_TRANSPOSE_MODELVIEW_MATRIX_NV
          GL_PATH_PROJECTION_STACK_DEPTH_NV
          GL_PATH_PROJECTION_MATRIX_NV
          GL_PATH_MAX_PROJECTION_STACK_DEPTH_NV
          GL_PATH_TRANSPOSE_PROJECTION_MATRIX_NV
          GL_FRAGMENT_INPUT_NV
          GL_SHARED_EDGE_NV
          GL_REPRESENTATIVE_FRAGMENT_TEST_NV
          GL_SAMPLE_LOCATION_SUBPIXEL_BITS_NV
          GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_NV
          GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_NV
          GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_NV
          GL_SAMPLE_LOCATION_NV
          GL_PROGRAMMABLE_SAMPLE_LOCATION_NV
          GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_NV
          GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_NV
          GL_SCISSOR_TEST_EXCLUSIVE_NV
          GL_SCISSOR_BOX_EXCLUSIVE_NV
          GL_BUFFER_GPU_ADDRESS_NV
          GL_GPU_ADDRESS_NV
          GL_MAX_SHADER_BUFFER_ADDRESS_NV
          GL_SHADER_GLOBAL_ACCESS_BARRIER_BIT_NV
          GL_SUBGROUP_FEATURE_PARTITIONED_BIT_NV
          GL_WARP_SIZE_NV
          GL_WARPS_PER_SM_NV
          GL_SM_COUNT_NV
          GL_SHADING_RATE_IMAGE_NV
          GL_SHADING_RATE_NO_INVOCATIONS_NV
          GL_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV
          GL_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV
          GL_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV
          GL_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV
          GL_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV
          GL_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV
          GL_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV
          GL_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV
          GL_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV
          GL_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV
          GL_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV
          GL_SHADING_RATE_IMAGE_BINDING_NV
          GL_SHADING_RATE_IMAGE_TEXEL_WIDTH_NV
          GL_SHADING_RATE_IMAGE_TEXEL_HEIGHT_NV
          GL_SHADING_RATE_IMAGE_PALETTE_SIZE_NV
          GL_MAX_COARSE_FRAGMENT_SAMPLES_NV
          GL_SHADING_RATE_SAMPLE_ORDER_DEFAULT_NV
          GL_SHADING_RATE_SAMPLE_ORDER_PIXEL_MAJOR_NV
          GL_SHADING_RATE_SAMPLE_ORDER_SAMPLE_MAJOR_NV
          GL_UNIFORM_BUFFER_UNIFIED_NV
          GL_UNIFORM_BUFFER_ADDRESS_NV
          GL_UNIFORM_BUFFER_LENGTH_NV
          GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV
          GL_ELEMENT_ARRAY_UNIFIED_NV
          GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV
          GL_VERTEX_ARRAY_ADDRESS_NV
          GL_NORMAL_ARRAY_ADDRESS_NV
          GL_COLOR_ARRAY_ADDRESS_NV
          GL_INDEX_ARRAY_ADDRESS_NV
          GL_TEXTURE_COORD_ARRAY_ADDRESS_NV
          GL_EDGE_FLAG_ARRAY_ADDRESS_NV
          GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV
          GL_FOG_COORD_ARRAY_ADDRESS_NV
          GL_ELEMENT_ARRAY_ADDRESS_NV
          GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV
          GL_VERTEX_ARRAY_LENGTH_NV
          GL_NORMAL_ARRAY_LENGTH_NV
          GL_COLOR_ARRAY_LENGTH_NV
          GL_INDEX_ARRAY_LENGTH_NV
          GL_TEXTURE_COORD_ARRAY_LENGTH_NV
          GL_EDGE_FLAG_ARRAY_LENGTH_NV
          GL_SECONDARY_COLOR_ARRAY_LENGTH_NV
          GL_FOG_COORD_ARRAY_LENGTH_NV
          GL_ELEMENT_ARRAY_LENGTH_NV
          GL_DRAW_INDIRECT_UNIFIED_NV
          GL_DRAW_INDIRECT_ADDRESS_NV
          GL_DRAW_INDIRECT_LENGTH_NV
          GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV
          GL_VIEWPORT_SWIZZLE_NEGATIVE_X_NV
          GL_VIEWPORT_SWIZZLE_POSITIVE_Y_NV
          GL_VIEWPORT_SWIZZLE_NEGATIVE_Y_NV
          GL_VIEWPORT_SWIZZLE_POSITIVE_Z_NV
          GL_VIEWPORT_SWIZZLE_NEGATIVE_Z_NV
          GL_VIEWPORT_SWIZZLE_POSITIVE_W_NV
          GL_VIEWPORT_SWIZZLE_NEGATIVE_W_NV
          GL_VIEWPORT_SWIZZLE_X_NV
          GL_VIEWPORT_SWIZZLE_Y_NV
          GL_VIEWPORT_SWIZZLE_Z_NV
          GL_VIEWPORT_SWIZZLE_W_NV
          GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_NUM_VIEWS_OVR
          GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_BASE_VIEW_INDEX_OVR
          GL_MAX_VIEWS_OVR
          GL_FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR)
  (import (core) (ypsilon c-ffi))
  (define libGL
    (let ((sysname (architecture-feature 'sysname)))
      (cond ((string-contains sysname "darwin")
             (load-shared-object "OpenGL.framework/OpenGL"))
            ((string-contains sysname "linux")
             (load-shared-object "libGL.so.1"))
            (else
              (assertion-violation 'load-shared-object "can not load GL library, unknown operating system")))))
  (define-syntax define-cdecl
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function/weak ret name args)))))
  (define GL_VERSION_1_0 1)
  (define GL_DEPTH_BUFFER_BIT #x00000100)
  (define GL_STENCIL_BUFFER_BIT #x00000400)
  (define GL_COLOR_BUFFER_BIT #x00004000)
  (define GL_FALSE 0)
  (define GL_TRUE 1)
  (define GL_POINTS #x0000)
  (define GL_LINES #x0001)
  (define GL_LINE_LOOP #x0002)
  (define GL_LINE_STRIP #x0003)
  (define GL_TRIANGLES #x0004)
  (define GL_TRIANGLE_STRIP #x0005)
  (define GL_TRIANGLE_FAN #x0006)
  (define GL_QUADS #x0007)
  (define GL_NEVER #x0200)
  (define GL_LESS #x0201)
  (define GL_EQUAL #x0202)
  (define GL_LEQUAL #x0203)
  (define GL_GREATER #x0204)
  (define GL_NOTEQUAL #x0205)
  (define GL_GEQUAL #x0206)
  (define GL_ALWAYS #x0207)
  (define GL_ZERO 0)
  (define GL_ONE 1)
  (define GL_SRC_COLOR #x0300)
  (define GL_ONE_MINUS_SRC_COLOR #x0301)
  (define GL_SRC_ALPHA #x0302)
  (define GL_ONE_MINUS_SRC_ALPHA #x0303)
  (define GL_DST_ALPHA #x0304)
  (define GL_ONE_MINUS_DST_ALPHA #x0305)
  (define GL_DST_COLOR #x0306)
  (define GL_ONE_MINUS_DST_COLOR #x0307)
  (define GL_SRC_ALPHA_SATURATE #x0308)
  (define GL_NONE 0)
  (define GL_FRONT_LEFT #x0400)
  (define GL_FRONT_RIGHT #x0401)
  (define GL_BACK_LEFT #x0402)
  (define GL_BACK_RIGHT #x0403)
  (define GL_FRONT #x0404)
  (define GL_BACK #x0405)
  (define GL_LEFT #x0406)
  (define GL_RIGHT #x0407)
  (define GL_FRONT_AND_BACK #x0408)
  (define GL_NO_ERROR 0)
  (define GL_INVALID_ENUM #x0500)
  (define GL_INVALID_VALUE #x0501)
  (define GL_INVALID_OPERATION #x0502)
  (define GL_OUT_OF_MEMORY #x0505)
  (define GL_CW #x0900)
  (define GL_CCW #x0901)
  (define GL_POINT_SIZE #x0B11)
  (define GL_POINT_SIZE_RANGE #x0B12)
  (define GL_POINT_SIZE_GRANULARITY #x0B13)
  (define GL_LINE_SMOOTH #x0B20)
  (define GL_LINE_WIDTH #x0B21)
  (define GL_LINE_WIDTH_RANGE #x0B22)
  (define GL_LINE_WIDTH_GRANULARITY #x0B23)
  (define GL_POLYGON_MODE #x0B40)
  (define GL_POLYGON_SMOOTH #x0B41)
  (define GL_CULL_FACE #x0B44)
  (define GL_CULL_FACE_MODE #x0B45)
  (define GL_FRONT_FACE #x0B46)
  (define GL_DEPTH_RANGE #x0B70)
  (define GL_DEPTH_TEST #x0B71)
  (define GL_DEPTH_WRITEMASK #x0B72)
  (define GL_DEPTH_CLEAR_VALUE #x0B73)
  (define GL_DEPTH_FUNC #x0B74)
  (define GL_STENCIL_TEST #x0B90)
  (define GL_STENCIL_CLEAR_VALUE #x0B91)
  (define GL_STENCIL_FUNC #x0B92)
  (define GL_STENCIL_VALUE_MASK #x0B93)
  (define GL_STENCIL_FAIL #x0B94)
  (define GL_STENCIL_PASS_DEPTH_FAIL #x0B95)
  (define GL_STENCIL_PASS_DEPTH_PASS #x0B96)
  (define GL_STENCIL_REF #x0B97)
  (define GL_STENCIL_WRITEMASK #x0B98)
  (define GL_VIEWPORT #x0BA2)
  (define GL_DITHER #x0BD0)
  (define GL_BLEND_DST #x0BE0)
  (define GL_BLEND_SRC #x0BE1)
  (define GL_BLEND #x0BE2)
  (define GL_LOGIC_OP_MODE #x0BF0)
  (define GL_DRAW_BUFFER #x0C01)
  (define GL_READ_BUFFER #x0C02)
  (define GL_SCISSOR_BOX #x0C10)
  (define GL_SCISSOR_TEST #x0C11)
  (define GL_COLOR_CLEAR_VALUE #x0C22)
  (define GL_COLOR_WRITEMASK #x0C23)
  (define GL_DOUBLEBUFFER #x0C32)
  (define GL_STEREO #x0C33)
  (define GL_LINE_SMOOTH_HINT #x0C52)
  (define GL_POLYGON_SMOOTH_HINT #x0C53)
  (define GL_UNPACK_SWAP_BYTES #x0CF0)
  (define GL_UNPACK_LSB_FIRST #x0CF1)
  (define GL_UNPACK_ROW_LENGTH #x0CF2)
  (define GL_UNPACK_SKIP_ROWS #x0CF3)
  (define GL_UNPACK_SKIP_PIXELS #x0CF4)
  (define GL_UNPACK_ALIGNMENT #x0CF5)
  (define GL_PACK_SWAP_BYTES #x0D00)
  (define GL_PACK_LSB_FIRST #x0D01)
  (define GL_PACK_ROW_LENGTH #x0D02)
  (define GL_PACK_SKIP_ROWS #x0D03)
  (define GL_PACK_SKIP_PIXELS #x0D04)
  (define GL_PACK_ALIGNMENT #x0D05)
  (define GL_MAX_TEXTURE_SIZE #x0D33)
  (define GL_MAX_VIEWPORT_DIMS #x0D3A)
  (define GL_SUBPIXEL_BITS #x0D50)
  (define GL_TEXTURE_1D #x0DE0)
  (define GL_TEXTURE_2D #x0DE1)
  (define GL_TEXTURE_WIDTH #x1000)
  (define GL_TEXTURE_HEIGHT #x1001)
  (define GL_TEXTURE_BORDER_COLOR #x1004)
  (define GL_DONT_CARE #x1100)
  (define GL_FASTEST #x1101)
  (define GL_NICEST #x1102)
  (define GL_BYTE #x1400)
  (define GL_UNSIGNED_BYTE #x1401)
  (define GL_SHORT #x1402)
  (define GL_UNSIGNED_SHORT #x1403)
  (define GL_INT #x1404)
  (define GL_UNSIGNED_INT #x1405)
  (define GL_FLOAT #x1406)
  (define GL_STACK_OVERFLOW #x0503)
  (define GL_STACK_UNDERFLOW #x0504)
  (define GL_CLEAR #x1500)
  (define GL_AND #x1501)
  (define GL_AND_REVERSE #x1502)
  (define GL_COPY #x1503)
  (define GL_AND_INVERTED #x1504)
  (define GL_NOOP #x1505)
  (define GL_XOR #x1506)
  (define GL_OR #x1507)
  (define GL_NOR #x1508)
  (define GL_EQUIV #x1509)
  (define GL_INVERT #x150A)
  (define GL_OR_REVERSE #x150B)
  (define GL_COPY_INVERTED #x150C)
  (define GL_OR_INVERTED #x150D)
  (define GL_NAND #x150E)
  (define GL_SET #x150F)
  (define GL_TEXTURE #x1702)
  (define GL_COLOR #x1800)
  (define GL_DEPTH #x1801)
  (define GL_STENCIL #x1802)
  (define GL_STENCIL_INDEX #x1901)
  (define GL_DEPTH_COMPONENT #x1902)
  (define GL_RED #x1903)
  (define GL_GREEN #x1904)
  (define GL_BLUE #x1905)
  (define GL_ALPHA #x1906)
  (define GL_RGB #x1907)
  (define GL_RGBA #x1908)
  (define GL_POINT #x1B00)
  (define GL_LINE #x1B01)
  (define GL_FILL #x1B02)
  (define GL_KEEP #x1E00)
  (define GL_REPLACE #x1E01)
  (define GL_INCR #x1E02)
  (define GL_DECR #x1E03)
  (define GL_VENDOR #x1F00)
  (define GL_RENDERER #x1F01)
  (define GL_VERSION #x1F02)
  (define GL_EXTENSIONS #x1F03)
  (define GL_NEAREST #x2600)
  (define GL_LINEAR #x2601)
  (define GL_NEAREST_MIPMAP_NEAREST #x2700)
  (define GL_LINEAR_MIPMAP_NEAREST #x2701)
  (define GL_NEAREST_MIPMAP_LINEAR #x2702)
  (define GL_LINEAR_MIPMAP_LINEAR #x2703)
  (define GL_TEXTURE_MAG_FILTER #x2800)
  (define GL_TEXTURE_MIN_FILTER #x2801)
  (define GL_TEXTURE_WRAP_S #x2802)
  (define GL_TEXTURE_WRAP_T #x2803)
  (define GL_REPEAT #x2901)
  (define GL_VERSION_1_1 1)
  (define GL_COLOR_LOGIC_OP #x0BF2)
  (define GL_POLYGON_OFFSET_UNITS #x2A00)
  (define GL_POLYGON_OFFSET_POINT #x2A01)
  (define GL_POLYGON_OFFSET_LINE #x2A02)
  (define GL_POLYGON_OFFSET_FILL #x8037)
  (define GL_POLYGON_OFFSET_FACTOR #x8038)
  (define GL_TEXTURE_BINDING_1D #x8068)
  (define GL_TEXTURE_BINDING_2D #x8069)
  (define GL_TEXTURE_INTERNAL_FORMAT #x1003)
  (define GL_TEXTURE_RED_SIZE #x805C)
  (define GL_TEXTURE_GREEN_SIZE #x805D)
  (define GL_TEXTURE_BLUE_SIZE #x805E)
  (define GL_TEXTURE_ALPHA_SIZE #x805F)
  (define GL_DOUBLE #x140A)
  (define GL_PROXY_TEXTURE_1D #x8063)
  (define GL_PROXY_TEXTURE_2D #x8064)
  (define GL_R3_G3_B2 #x2A10)
  (define GL_RGB4 #x804F)
  (define GL_RGB5 #x8050)
  (define GL_RGB8 #x8051)
  (define GL_RGB10 #x8052)
  (define GL_RGB12 #x8053)
  (define GL_RGB16 #x8054)
  (define GL_RGBA2 #x8055)
  (define GL_RGBA4 #x8056)
  (define GL_RGB5_A1 #x8057)
  (define GL_RGBA8 #x8058)
  (define GL_RGB10_A2 #x8059)
  (define GL_RGBA12 #x805A)
  (define GL_RGBA16 #x805B)
  (define GL_VERTEX_ARRAY #x8074)
  (define GL_VERSION_1_2 1)
  (define GL_UNSIGNED_BYTE_3_3_2 #x8032)
  (define GL_UNSIGNED_SHORT_4_4_4_4 #x8033)
  (define GL_UNSIGNED_SHORT_5_5_5_1 #x8034)
  (define GL_UNSIGNED_INT_8_8_8_8 #x8035)
  (define GL_UNSIGNED_INT_10_10_10_2 #x8036)
  (define GL_TEXTURE_BINDING_3D #x806A)
  (define GL_PACK_SKIP_IMAGES #x806B)
  (define GL_PACK_IMAGE_HEIGHT #x806C)
  (define GL_UNPACK_SKIP_IMAGES #x806D)
  (define GL_UNPACK_IMAGE_HEIGHT #x806E)
  (define GL_TEXTURE_3D #x806F)
  (define GL_PROXY_TEXTURE_3D #x8070)
  (define GL_TEXTURE_DEPTH #x8071)
  (define GL_TEXTURE_WRAP_R #x8072)
  (define GL_MAX_3D_TEXTURE_SIZE #x8073)
  (define GL_UNSIGNED_BYTE_2_3_3_REV #x8362)
  (define GL_UNSIGNED_SHORT_5_6_5 #x8363)
  (define GL_UNSIGNED_SHORT_5_6_5_REV #x8364)
  (define GL_UNSIGNED_SHORT_4_4_4_4_REV #x8365)
  (define GL_UNSIGNED_SHORT_1_5_5_5_REV #x8366)
  (define GL_UNSIGNED_INT_8_8_8_8_REV #x8367)
  (define GL_UNSIGNED_INT_2_10_10_10_REV #x8368)
  (define GL_BGR #x80E0)
  (define GL_BGRA #x80E1)
  (define GL_MAX_ELEMENTS_VERTICES #x80E8)
  (define GL_MAX_ELEMENTS_INDICES #x80E9)
  (define GL_CLAMP_TO_EDGE #x812F)
  (define GL_TEXTURE_MIN_LOD #x813A)
  (define GL_TEXTURE_MAX_LOD #x813B)
  (define GL_TEXTURE_BASE_LEVEL #x813C)
  (define GL_TEXTURE_MAX_LEVEL #x813D)
  (define GL_SMOOTH_POINT_SIZE_RANGE #x0B12)
  (define GL_SMOOTH_POINT_SIZE_GRANULARITY #x0B13)
  (define GL_SMOOTH_LINE_WIDTH_RANGE #x0B22)
  (define GL_SMOOTH_LINE_WIDTH_GRANULARITY #x0B23)
  (define GL_ALIASED_LINE_WIDTH_RANGE #x846E)
  (define GL_VERSION_1_3 1)
  (define GL_TEXTURE0 #x84C0)
  (define GL_TEXTURE1 #x84C1)
  (define GL_TEXTURE2 #x84C2)
  (define GL_TEXTURE3 #x84C3)
  (define GL_TEXTURE4 #x84C4)
  (define GL_TEXTURE5 #x84C5)
  (define GL_TEXTURE6 #x84C6)
  (define GL_TEXTURE7 #x84C7)
  (define GL_TEXTURE8 #x84C8)
  (define GL_TEXTURE9 #x84C9)
  (define GL_TEXTURE10 #x84CA)
  (define GL_TEXTURE11 #x84CB)
  (define GL_TEXTURE12 #x84CC)
  (define GL_TEXTURE13 #x84CD)
  (define GL_TEXTURE14 #x84CE)
  (define GL_TEXTURE15 #x84CF)
  (define GL_TEXTURE16 #x84D0)
  (define GL_TEXTURE17 #x84D1)
  (define GL_TEXTURE18 #x84D2)
  (define GL_TEXTURE19 #x84D3)
  (define GL_TEXTURE20 #x84D4)
  (define GL_TEXTURE21 #x84D5)
  (define GL_TEXTURE22 #x84D6)
  (define GL_TEXTURE23 #x84D7)
  (define GL_TEXTURE24 #x84D8)
  (define GL_TEXTURE25 #x84D9)
  (define GL_TEXTURE26 #x84DA)
  (define GL_TEXTURE27 #x84DB)
  (define GL_TEXTURE28 #x84DC)
  (define GL_TEXTURE29 #x84DD)
  (define GL_TEXTURE30 #x84DE)
  (define GL_TEXTURE31 #x84DF)
  (define GL_ACTIVE_TEXTURE #x84E0)
  (define GL_MULTISAMPLE #x809D)
  (define GL_SAMPLE_ALPHA_TO_COVERAGE #x809E)
  (define GL_SAMPLE_ALPHA_TO_ONE #x809F)
  (define GL_SAMPLE_COVERAGE #x80A0)
  (define GL_SAMPLE_BUFFERS #x80A8)
  (define GL_SAMPLES #x80A9)
  (define GL_SAMPLE_COVERAGE_VALUE #x80AA)
  (define GL_SAMPLE_COVERAGE_INVERT #x80AB)
  (define GL_TEXTURE_CUBE_MAP #x8513)
  (define GL_TEXTURE_BINDING_CUBE_MAP #x8514)
  (define GL_TEXTURE_CUBE_MAP_POSITIVE_X #x8515)
  (define GL_TEXTURE_CUBE_MAP_NEGATIVE_X #x8516)
  (define GL_TEXTURE_CUBE_MAP_POSITIVE_Y #x8517)
  (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y #x8518)
  (define GL_TEXTURE_CUBE_MAP_POSITIVE_Z #x8519)
  (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z #x851A)
  (define GL_PROXY_TEXTURE_CUBE_MAP #x851B)
  (define GL_MAX_CUBE_MAP_TEXTURE_SIZE #x851C)
  (define GL_COMPRESSED_RGB #x84ED)
  (define GL_COMPRESSED_RGBA #x84EE)
  (define GL_TEXTURE_COMPRESSION_HINT #x84EF)
  (define GL_TEXTURE_COMPRESSED_IMAGE_SIZE #x86A0)
  (define GL_TEXTURE_COMPRESSED #x86A1)
  (define GL_NUM_COMPRESSED_TEXTURE_FORMATS #x86A2)
  (define GL_COMPRESSED_TEXTURE_FORMATS #x86A3)
  (define GL_CLAMP_TO_BORDER #x812D)
  (define GL_VERSION_1_4 1)
  (define GL_BLEND_DST_RGB #x80C8)
  (define GL_BLEND_SRC_RGB #x80C9)
  (define GL_BLEND_DST_ALPHA #x80CA)
  (define GL_BLEND_SRC_ALPHA #x80CB)
  (define GL_POINT_FADE_THRESHOLD_SIZE #x8128)
  (define GL_DEPTH_COMPONENT16 #x81A5)
  (define GL_DEPTH_COMPONENT24 #x81A6)
  (define GL_DEPTH_COMPONENT32 #x81A7)
  (define GL_MIRRORED_REPEAT #x8370)
  (define GL_MAX_TEXTURE_LOD_BIAS #x84FD)
  (define GL_TEXTURE_LOD_BIAS #x8501)
  (define GL_INCR_WRAP #x8507)
  (define GL_DECR_WRAP #x8508)
  (define GL_TEXTURE_DEPTH_SIZE #x884A)
  (define GL_TEXTURE_COMPARE_MODE #x884C)
  (define GL_TEXTURE_COMPARE_FUNC #x884D)
  (define GL_BLEND_COLOR #x8005)
  (define GL_BLEND_EQUATION #x8009)
  (define GL_CONSTANT_COLOR #x8001)
  (define GL_ONE_MINUS_CONSTANT_COLOR #x8002)
  (define GL_CONSTANT_ALPHA #x8003)
  (define GL_ONE_MINUS_CONSTANT_ALPHA #x8004)
  (define GL_FUNC_ADD #x8006)
  (define GL_FUNC_REVERSE_SUBTRACT #x800B)
  (define GL_FUNC_SUBTRACT #x800A)
  (define GL_MIN #x8007)
  (define GL_MAX #x8008)
  (define GL_VERSION_1_5 1)
  (define GL_BUFFER_SIZE #x8764)
  (define GL_BUFFER_USAGE #x8765)
  (define GL_QUERY_COUNTER_BITS #x8864)
  (define GL_CURRENT_QUERY #x8865)
  (define GL_QUERY_RESULT #x8866)
  (define GL_QUERY_RESULT_AVAILABLE #x8867)
  (define GL_ARRAY_BUFFER #x8892)
  (define GL_ELEMENT_ARRAY_BUFFER #x8893)
  (define GL_ARRAY_BUFFER_BINDING #x8894)
  (define GL_ELEMENT_ARRAY_BUFFER_BINDING #x8895)
  (define GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING #x889F)
  (define GL_READ_ONLY #x88B8)
  (define GL_WRITE_ONLY #x88B9)
  (define GL_READ_WRITE #x88BA)
  (define GL_BUFFER_ACCESS #x88BB)
  (define GL_BUFFER_MAPPED #x88BC)
  (define GL_BUFFER_MAP_POINTER #x88BD)
  (define GL_STREAM_DRAW #x88E0)
  (define GL_STREAM_READ #x88E1)
  (define GL_STREAM_COPY #x88E2)
  (define GL_STATIC_DRAW #x88E4)
  (define GL_STATIC_READ #x88E5)
  (define GL_STATIC_COPY #x88E6)
  (define GL_DYNAMIC_DRAW #x88E8)
  (define GL_DYNAMIC_READ #x88E9)
  (define GL_DYNAMIC_COPY #x88EA)
  (define GL_SAMPLES_PASSED #x8914)
  (define GL_SRC1_ALPHA #x8589)
  (define GL_VERSION_2_0 1)
  (define GL_BLEND_EQUATION_RGB #x8009)
  (define GL_VERTEX_ATTRIB_ARRAY_ENABLED #x8622)
  (define GL_VERTEX_ATTRIB_ARRAY_SIZE #x8623)
  (define GL_VERTEX_ATTRIB_ARRAY_STRIDE #x8624)
  (define GL_VERTEX_ATTRIB_ARRAY_TYPE #x8625)
  (define GL_CURRENT_VERTEX_ATTRIB #x8626)
  (define GL_VERTEX_PROGRAM_POINT_SIZE #x8642)
  (define GL_VERTEX_ATTRIB_ARRAY_POINTER #x8645)
  (define GL_STENCIL_BACK_FUNC #x8800)
  (define GL_STENCIL_BACK_FAIL #x8801)
  (define GL_STENCIL_BACK_PASS_DEPTH_FAIL #x8802)
  (define GL_STENCIL_BACK_PASS_DEPTH_PASS #x8803)
  (define GL_MAX_DRAW_BUFFERS #x8824)
  (define GL_DRAW_BUFFER0 #x8825)
  (define GL_DRAW_BUFFER1 #x8826)
  (define GL_DRAW_BUFFER2 #x8827)
  (define GL_DRAW_BUFFER3 #x8828)
  (define GL_DRAW_BUFFER4 #x8829)
  (define GL_DRAW_BUFFER5 #x882A)
  (define GL_DRAW_BUFFER6 #x882B)
  (define GL_DRAW_BUFFER7 #x882C)
  (define GL_DRAW_BUFFER8 #x882D)
  (define GL_DRAW_BUFFER9 #x882E)
  (define GL_DRAW_BUFFER10 #x882F)
  (define GL_DRAW_BUFFER11 #x8830)
  (define GL_DRAW_BUFFER12 #x8831)
  (define GL_DRAW_BUFFER13 #x8832)
  (define GL_DRAW_BUFFER14 #x8833)
  (define GL_DRAW_BUFFER15 #x8834)
  (define GL_BLEND_EQUATION_ALPHA #x883D)
  (define GL_MAX_VERTEX_ATTRIBS #x8869)
  (define GL_VERTEX_ATTRIB_ARRAY_NORMALIZED #x886A)
  (define GL_MAX_TEXTURE_IMAGE_UNITS #x8872)
  (define GL_FRAGMENT_SHADER #x8B30)
  (define GL_VERTEX_SHADER #x8B31)
  (define GL_MAX_FRAGMENT_UNIFORM_COMPONENTS #x8B49)
  (define GL_MAX_VERTEX_UNIFORM_COMPONENTS #x8B4A)
  (define GL_MAX_VARYING_FLOATS #x8B4B)
  (define GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS #x8B4C)
  (define GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS #x8B4D)
  (define GL_SHADER_TYPE #x8B4F)
  (define GL_FLOAT_VEC2 #x8B50)
  (define GL_FLOAT_VEC3 #x8B51)
  (define GL_FLOAT_VEC4 #x8B52)
  (define GL_INT_VEC2 #x8B53)
  (define GL_INT_VEC3 #x8B54)
  (define GL_INT_VEC4 #x8B55)
  (define GL_BOOL #x8B56)
  (define GL_BOOL_VEC2 #x8B57)
  (define GL_BOOL_VEC3 #x8B58)
  (define GL_BOOL_VEC4 #x8B59)
  (define GL_FLOAT_MAT2 #x8B5A)
  (define GL_FLOAT_MAT3 #x8B5B)
  (define GL_FLOAT_MAT4 #x8B5C)
  (define GL_SAMPLER_1D #x8B5D)
  (define GL_SAMPLER_2D #x8B5E)
  (define GL_SAMPLER_3D #x8B5F)
  (define GL_SAMPLER_CUBE #x8B60)
  (define GL_SAMPLER_1D_SHADOW #x8B61)
  (define GL_SAMPLER_2D_SHADOW #x8B62)
  (define GL_DELETE_STATUS #x8B80)
  (define GL_COMPILE_STATUS #x8B81)
  (define GL_LINK_STATUS #x8B82)
  (define GL_VALIDATE_STATUS #x8B83)
  (define GL_INFO_LOG_LENGTH #x8B84)
  (define GL_ATTACHED_SHADERS #x8B85)
  (define GL_ACTIVE_UNIFORMS #x8B86)
  (define GL_ACTIVE_UNIFORM_MAX_LENGTH #x8B87)
  (define GL_SHADER_SOURCE_LENGTH #x8B88)
  (define GL_ACTIVE_ATTRIBUTES #x8B89)
  (define GL_ACTIVE_ATTRIBUTE_MAX_LENGTH #x8B8A)
  (define GL_FRAGMENT_SHADER_DERIVATIVE_HINT #x8B8B)
  (define GL_SHADING_LANGUAGE_VERSION #x8B8C)
  (define GL_CURRENT_PROGRAM #x8B8D)
  (define GL_POINT_SPRITE_COORD_ORIGIN #x8CA0)
  (define GL_LOWER_LEFT #x8CA1)
  (define GL_UPPER_LEFT #x8CA2)
  (define GL_STENCIL_BACK_REF #x8CA3)
  (define GL_STENCIL_BACK_VALUE_MASK #x8CA4)
  (define GL_STENCIL_BACK_WRITEMASK #x8CA5)
  (define GL_VERSION_2_1 1)
  (define GL_PIXEL_PACK_BUFFER #x88EB)
  (define GL_PIXEL_UNPACK_BUFFER #x88EC)
  (define GL_PIXEL_PACK_BUFFER_BINDING #x88ED)
  (define GL_PIXEL_UNPACK_BUFFER_BINDING #x88EF)
  (define GL_SRGB #x8C40)
  (define GL_SRGB8 #x8C41)
  (define GL_SRGB_ALPHA #x8C42)
  (define GL_SRGB8_ALPHA8 #x8C43)
  (define GL_COMPRESSED_SRGB #x8C48)
  (define GL_COMPRESSED_SRGB_ALPHA #x8C49)
  (define GL_VERSION_3_0 1)
  (define GL_COMPARE_REF_TO_TEXTURE #x884E)
  (define GL_CLIP_DISTANCE0 #x3000)
  (define GL_CLIP_DISTANCE1 #x3001)
  (define GL_CLIP_DISTANCE2 #x3002)
  (define GL_CLIP_DISTANCE3 #x3003)
  (define GL_CLIP_DISTANCE4 #x3004)
  (define GL_CLIP_DISTANCE5 #x3005)
  (define GL_CLIP_DISTANCE6 #x3006)
  (define GL_CLIP_DISTANCE7 #x3007)
  (define GL_MAX_CLIP_DISTANCES #x0D32)
  (define GL_MAJOR_VERSION #x821B)
  (define GL_MINOR_VERSION #x821C)
  (define GL_NUM_EXTENSIONS #x821D)
  (define GL_CONTEXT_FLAGS #x821E)
  (define GL_COMPRESSED_RED #x8225)
  (define GL_COMPRESSED_RG #x8226)
  (define GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT #x00000001)
  (define GL_RGBA32F #x8814)
  (define GL_RGB32F #x8815)
  (define GL_RGBA16F #x881A)
  (define GL_RGB16F #x881B)
  (define GL_VERTEX_ATTRIB_ARRAY_INTEGER #x88FD)
  (define GL_MAX_ARRAY_TEXTURE_LAYERS #x88FF)
  (define GL_MIN_PROGRAM_TEXEL_OFFSET #x8904)
  (define GL_MAX_PROGRAM_TEXEL_OFFSET #x8905)
  (define GL_CLAMP_READ_COLOR #x891C)
  (define GL_FIXED_ONLY #x891D)
  (define GL_MAX_VARYING_COMPONENTS #x8B4B)
  (define GL_TEXTURE_1D_ARRAY #x8C18)
  (define GL_PROXY_TEXTURE_1D_ARRAY #x8C19)
  (define GL_TEXTURE_2D_ARRAY #x8C1A)
  (define GL_PROXY_TEXTURE_2D_ARRAY #x8C1B)
  (define GL_TEXTURE_BINDING_1D_ARRAY #x8C1C)
  (define GL_TEXTURE_BINDING_2D_ARRAY #x8C1D)
  (define GL_R11F_G11F_B10F #x8C3A)
  (define GL_UNSIGNED_INT_10F_11F_11F_REV #x8C3B)
  (define GL_RGB9_E5 #x8C3D)
  (define GL_UNSIGNED_INT_5_9_9_9_REV #x8C3E)
  (define GL_TEXTURE_SHARED_SIZE #x8C3F)
  (define GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH #x8C76)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_MODE #x8C7F)
  (define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS #x8C80)
  (define GL_TRANSFORM_FEEDBACK_VARYINGS #x8C83)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_START #x8C84)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_SIZE #x8C85)
  (define GL_PRIMITIVES_GENERATED #x8C87)
  (define GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN #x8C88)
  (define GL_RASTERIZER_DISCARD #x8C89)
  (define GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS #x8C8A)
  (define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS #x8C8B)
  (define GL_INTERLEAVED_ATTRIBS #x8C8C)
  (define GL_SEPARATE_ATTRIBS #x8C8D)
  (define GL_TRANSFORM_FEEDBACK_BUFFER #x8C8E)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_BINDING #x8C8F)
  (define GL_RGBA32UI #x8D70)
  (define GL_RGB32UI #x8D71)
  (define GL_RGBA16UI #x8D76)
  (define GL_RGB16UI #x8D77)
  (define GL_RGBA8UI #x8D7C)
  (define GL_RGB8UI #x8D7D)
  (define GL_RGBA32I #x8D82)
  (define GL_RGB32I #x8D83)
  (define GL_RGBA16I #x8D88)
  (define GL_RGB16I #x8D89)
  (define GL_RGBA8I #x8D8E)
  (define GL_RGB8I #x8D8F)
  (define GL_RED_INTEGER #x8D94)
  (define GL_GREEN_INTEGER #x8D95)
  (define GL_BLUE_INTEGER #x8D96)
  (define GL_RGB_INTEGER #x8D98)
  (define GL_RGBA_INTEGER #x8D99)
  (define GL_BGR_INTEGER #x8D9A)
  (define GL_BGRA_INTEGER #x8D9B)
  (define GL_SAMPLER_1D_ARRAY #x8DC0)
  (define GL_SAMPLER_2D_ARRAY #x8DC1)
  (define GL_SAMPLER_1D_ARRAY_SHADOW #x8DC3)
  (define GL_SAMPLER_2D_ARRAY_SHADOW #x8DC4)
  (define GL_SAMPLER_CUBE_SHADOW #x8DC5)
  (define GL_UNSIGNED_INT_VEC2 #x8DC6)
  (define GL_UNSIGNED_INT_VEC3 #x8DC7)
  (define GL_UNSIGNED_INT_VEC4 #x8DC8)
  (define GL_INT_SAMPLER_1D #x8DC9)
  (define GL_INT_SAMPLER_2D #x8DCA)
  (define GL_INT_SAMPLER_3D #x8DCB)
  (define GL_INT_SAMPLER_CUBE #x8DCC)
  (define GL_INT_SAMPLER_1D_ARRAY #x8DCE)
  (define GL_INT_SAMPLER_2D_ARRAY #x8DCF)
  (define GL_UNSIGNED_INT_SAMPLER_1D #x8DD1)
  (define GL_UNSIGNED_INT_SAMPLER_2D #x8DD2)
  (define GL_UNSIGNED_INT_SAMPLER_3D #x8DD3)
  (define GL_UNSIGNED_INT_SAMPLER_CUBE #x8DD4)
  (define GL_UNSIGNED_INT_SAMPLER_1D_ARRAY #x8DD6)
  (define GL_UNSIGNED_INT_SAMPLER_2D_ARRAY #x8DD7)
  (define GL_QUERY_WAIT #x8E13)
  (define GL_QUERY_NO_WAIT #x8E14)
  (define GL_QUERY_BY_REGION_WAIT #x8E15)
  (define GL_QUERY_BY_REGION_NO_WAIT #x8E16)
  (define GL_BUFFER_ACCESS_FLAGS #x911F)
  (define GL_BUFFER_MAP_LENGTH #x9120)
  (define GL_BUFFER_MAP_OFFSET #x9121)
  (define GL_DEPTH_COMPONENT32F #x8CAC)
  (define GL_DEPTH32F_STENCIL8 #x8CAD)
  (define GL_FLOAT_32_UNSIGNED_INT_24_8_REV #x8DAD)
  (define GL_INVALID_FRAMEBUFFER_OPERATION #x0506)
  (define GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING #x8210)
  (define GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE #x8211)
  (define GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE #x8212)
  (define GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE #x8213)
  (define GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE #x8214)
  (define GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE #x8215)
  (define GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE #x8216)
  (define GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE #x8217)
  (define GL_FRAMEBUFFER_DEFAULT #x8218)
  (define GL_FRAMEBUFFER_UNDEFINED #x8219)
  (define GL_DEPTH_STENCIL_ATTACHMENT #x821A)
  (define GL_MAX_RENDERBUFFER_SIZE #x84E8)
  (define GL_DEPTH_STENCIL #x84F9)
  (define GL_UNSIGNED_INT_24_8 #x84FA)
  (define GL_DEPTH24_STENCIL8 #x88F0)
  (define GL_TEXTURE_STENCIL_SIZE #x88F1)
  (define GL_TEXTURE_RED_TYPE #x8C10)
  (define GL_TEXTURE_GREEN_TYPE #x8C11)
  (define GL_TEXTURE_BLUE_TYPE #x8C12)
  (define GL_TEXTURE_ALPHA_TYPE #x8C13)
  (define GL_TEXTURE_DEPTH_TYPE #x8C16)
  (define GL_UNSIGNED_NORMALIZED #x8C17)
  (define GL_FRAMEBUFFER_BINDING #x8CA6)
  (define GL_DRAW_FRAMEBUFFER_BINDING #x8CA6)
  (define GL_RENDERBUFFER_BINDING #x8CA7)
  (define GL_READ_FRAMEBUFFER #x8CA8)
  (define GL_DRAW_FRAMEBUFFER #x8CA9)
  (define GL_READ_FRAMEBUFFER_BINDING #x8CAA)
  (define GL_RENDERBUFFER_SAMPLES #x8CAB)
  (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE #x8CD0)
  (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME #x8CD1)
  (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL #x8CD2)
  (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE #x8CD3)
  (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER #x8CD4)
  (define GL_FRAMEBUFFER_COMPLETE #x8CD5)
  (define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT #x8CD6)
  (define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT #x8CD7)
  (define GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER #x8CDB)
  (define GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER #x8CDC)
  (define GL_FRAMEBUFFER_UNSUPPORTED #x8CDD)
  (define GL_MAX_COLOR_ATTACHMENTS #x8CDF)
  (define GL_COLOR_ATTACHMENT0 #x8CE0)
  (define GL_COLOR_ATTACHMENT1 #x8CE1)
  (define GL_COLOR_ATTACHMENT2 #x8CE2)
  (define GL_COLOR_ATTACHMENT3 #x8CE3)
  (define GL_COLOR_ATTACHMENT4 #x8CE4)
  (define GL_COLOR_ATTACHMENT5 #x8CE5)
  (define GL_COLOR_ATTACHMENT6 #x8CE6)
  (define GL_COLOR_ATTACHMENT7 #x8CE7)
  (define GL_COLOR_ATTACHMENT8 #x8CE8)
  (define GL_COLOR_ATTACHMENT9 #x8CE9)
  (define GL_COLOR_ATTACHMENT10 #x8CEA)
  (define GL_COLOR_ATTACHMENT11 #x8CEB)
  (define GL_COLOR_ATTACHMENT12 #x8CEC)
  (define GL_COLOR_ATTACHMENT13 #x8CED)
  (define GL_COLOR_ATTACHMENT14 #x8CEE)
  (define GL_COLOR_ATTACHMENT15 #x8CEF)
  (define GL_COLOR_ATTACHMENT16 #x8CF0)
  (define GL_COLOR_ATTACHMENT17 #x8CF1)
  (define GL_COLOR_ATTACHMENT18 #x8CF2)
  (define GL_COLOR_ATTACHMENT19 #x8CF3)
  (define GL_COLOR_ATTACHMENT20 #x8CF4)
  (define GL_COLOR_ATTACHMENT21 #x8CF5)
  (define GL_COLOR_ATTACHMENT22 #x8CF6)
  (define GL_COLOR_ATTACHMENT23 #x8CF7)
  (define GL_COLOR_ATTACHMENT24 #x8CF8)
  (define GL_COLOR_ATTACHMENT25 #x8CF9)
  (define GL_COLOR_ATTACHMENT26 #x8CFA)
  (define GL_COLOR_ATTACHMENT27 #x8CFB)
  (define GL_COLOR_ATTACHMENT28 #x8CFC)
  (define GL_COLOR_ATTACHMENT29 #x8CFD)
  (define GL_COLOR_ATTACHMENT30 #x8CFE)
  (define GL_COLOR_ATTACHMENT31 #x8CFF)
  (define GL_DEPTH_ATTACHMENT #x8D00)
  (define GL_STENCIL_ATTACHMENT #x8D20)
  (define GL_FRAMEBUFFER #x8D40)
  (define GL_RENDERBUFFER #x8D41)
  (define GL_RENDERBUFFER_WIDTH #x8D42)
  (define GL_RENDERBUFFER_HEIGHT #x8D43)
  (define GL_RENDERBUFFER_INTERNAL_FORMAT #x8D44)
  (define GL_STENCIL_INDEX1 #x8D46)
  (define GL_STENCIL_INDEX4 #x8D47)
  (define GL_STENCIL_INDEX8 #x8D48)
  (define GL_STENCIL_INDEX16 #x8D49)
  (define GL_RENDERBUFFER_RED_SIZE #x8D50)
  (define GL_RENDERBUFFER_GREEN_SIZE #x8D51)
  (define GL_RENDERBUFFER_BLUE_SIZE #x8D52)
  (define GL_RENDERBUFFER_ALPHA_SIZE #x8D53)
  (define GL_RENDERBUFFER_DEPTH_SIZE #x8D54)
  (define GL_RENDERBUFFER_STENCIL_SIZE #x8D55)
  (define GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE #x8D56)
  (define GL_MAX_SAMPLES #x8D57)
  (define GL_FRAMEBUFFER_SRGB #x8DB9)
  (define GL_HALF_FLOAT #x140B)
  (define GL_MAP_READ_BIT #x0001)
  (define GL_MAP_WRITE_BIT #x0002)
  (define GL_MAP_INVALIDATE_RANGE_BIT #x0004)
  (define GL_MAP_INVALIDATE_BUFFER_BIT #x0008)
  (define GL_MAP_FLUSH_EXPLICIT_BIT #x0010)
  (define GL_MAP_UNSYNCHRONIZED_BIT #x0020)
  (define GL_COMPRESSED_RED_RGTC1 #x8DBB)
  (define GL_COMPRESSED_SIGNED_RED_RGTC1 #x8DBC)
  (define GL_COMPRESSED_RG_RGTC2 #x8DBD)
  (define GL_COMPRESSED_SIGNED_RG_RGTC2 #x8DBE)
  (define GL_RG #x8227)
  (define GL_RG_INTEGER #x8228)
  (define GL_R8 #x8229)
  (define GL_R16 #x822A)
  (define GL_RG8 #x822B)
  (define GL_RG16 #x822C)
  (define GL_R16F #x822D)
  (define GL_R32F #x822E)
  (define GL_RG16F #x822F)
  (define GL_RG32F #x8230)
  (define GL_R8I #x8231)
  (define GL_R8UI #x8232)
  (define GL_R16I #x8233)
  (define GL_R16UI #x8234)
  (define GL_R32I #x8235)
  (define GL_R32UI #x8236)
  (define GL_RG8I #x8237)
  (define GL_RG8UI #x8238)
  (define GL_RG16I #x8239)
  (define GL_RG16UI #x823A)
  (define GL_RG32I #x823B)
  (define GL_RG32UI #x823C)
  (define GL_VERTEX_ARRAY_BINDING #x85B5)
  (define GL_VERSION_3_1 1)
  (define GL_SAMPLER_2D_RECT #x8B63)
  (define GL_SAMPLER_2D_RECT_SHADOW #x8B64)
  (define GL_SAMPLER_BUFFER #x8DC2)
  (define GL_INT_SAMPLER_2D_RECT #x8DCD)
  (define GL_INT_SAMPLER_BUFFER #x8DD0)
  (define GL_UNSIGNED_INT_SAMPLER_2D_RECT #x8DD5)
  (define GL_UNSIGNED_INT_SAMPLER_BUFFER #x8DD8)
  (define GL_TEXTURE_BUFFER #x8C2A)
  (define GL_MAX_TEXTURE_BUFFER_SIZE #x8C2B)
  (define GL_TEXTURE_BINDING_BUFFER #x8C2C)
  (define GL_TEXTURE_BUFFER_DATA_STORE_BINDING #x8C2D)
  (define GL_TEXTURE_RECTANGLE #x84F5)
  (define GL_TEXTURE_BINDING_RECTANGLE #x84F6)
  (define GL_PROXY_TEXTURE_RECTANGLE #x84F7)
  (define GL_MAX_RECTANGLE_TEXTURE_SIZE #x84F8)
  (define GL_R8_SNORM #x8F94)
  (define GL_RG8_SNORM #x8F95)
  (define GL_RGB8_SNORM #x8F96)
  (define GL_RGBA8_SNORM #x8F97)
  (define GL_R16_SNORM #x8F98)
  (define GL_RG16_SNORM #x8F99)
  (define GL_RGB16_SNORM #x8F9A)
  (define GL_RGBA16_SNORM #x8F9B)
  (define GL_SIGNED_NORMALIZED #x8F9C)
  (define GL_PRIMITIVE_RESTART #x8F9D)
  (define GL_PRIMITIVE_RESTART_INDEX #x8F9E)
  (define GL_COPY_READ_BUFFER #x8F36)
  (define GL_COPY_WRITE_BUFFER #x8F37)
  (define GL_UNIFORM_BUFFER #x8A11)
  (define GL_UNIFORM_BUFFER_BINDING #x8A28)
  (define GL_UNIFORM_BUFFER_START #x8A29)
  (define GL_UNIFORM_BUFFER_SIZE #x8A2A)
  (define GL_MAX_VERTEX_UNIFORM_BLOCKS #x8A2B)
  (define GL_MAX_GEOMETRY_UNIFORM_BLOCKS #x8A2C)
  (define GL_MAX_FRAGMENT_UNIFORM_BLOCKS #x8A2D)
  (define GL_MAX_COMBINED_UNIFORM_BLOCKS #x8A2E)
  (define GL_MAX_UNIFORM_BUFFER_BINDINGS #x8A2F)
  (define GL_MAX_UNIFORM_BLOCK_SIZE #x8A30)
  (define GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS #x8A31)
  (define GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS #x8A32)
  (define GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS #x8A33)
  (define GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT #x8A34)
  (define GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH #x8A35)
  (define GL_ACTIVE_UNIFORM_BLOCKS #x8A36)
  (define GL_UNIFORM_TYPE #x8A37)
  (define GL_UNIFORM_SIZE #x8A38)
  (define GL_UNIFORM_NAME_LENGTH #x8A39)
  (define GL_UNIFORM_BLOCK_INDEX #x8A3A)
  (define GL_UNIFORM_OFFSET #x8A3B)
  (define GL_UNIFORM_ARRAY_STRIDE #x8A3C)
  (define GL_UNIFORM_MATRIX_STRIDE #x8A3D)
  (define GL_UNIFORM_IS_ROW_MAJOR #x8A3E)
  (define GL_UNIFORM_BLOCK_BINDING #x8A3F)
  (define GL_UNIFORM_BLOCK_DATA_SIZE #x8A40)
  (define GL_UNIFORM_BLOCK_NAME_LENGTH #x8A41)
  (define GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS #x8A42)
  (define GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES #x8A43)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER #x8A44)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER #x8A45)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER #x8A46)
  (define GL_INVALID_INDEX #xFFFFFFFF)
  (define GL_VERSION_3_2 1)
  (define GL_CONTEXT_CORE_PROFILE_BIT #x00000001)
  (define GL_CONTEXT_COMPATIBILITY_PROFILE_BIT #x00000002)
  (define GL_LINES_ADJACENCY #x000A)
  (define GL_LINE_STRIP_ADJACENCY #x000B)
  (define GL_TRIANGLES_ADJACENCY #x000C)
  (define GL_TRIANGLE_STRIP_ADJACENCY #x000D)
  (define GL_PROGRAM_POINT_SIZE #x8642)
  (define GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS #x8C29)
  (define GL_FRAMEBUFFER_ATTACHMENT_LAYERED #x8DA7)
  (define GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS #x8DA8)
  (define GL_GEOMETRY_SHADER #x8DD9)
  (define GL_GEOMETRY_VERTICES_OUT #x8916)
  (define GL_GEOMETRY_INPUT_TYPE #x8917)
  (define GL_GEOMETRY_OUTPUT_TYPE #x8918)
  (define GL_MAX_GEOMETRY_UNIFORM_COMPONENTS #x8DDF)
  (define GL_MAX_GEOMETRY_OUTPUT_VERTICES #x8DE0)
  (define GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS #x8DE1)
  (define GL_MAX_VERTEX_OUTPUT_COMPONENTS #x9122)
  (define GL_MAX_GEOMETRY_INPUT_COMPONENTS #x9123)
  (define GL_MAX_GEOMETRY_OUTPUT_COMPONENTS #x9124)
  (define GL_MAX_FRAGMENT_INPUT_COMPONENTS #x9125)
  (define GL_CONTEXT_PROFILE_MASK #x9126)
  (define GL_DEPTH_CLAMP #x864F)
  (define GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION #x8E4C)
  (define GL_FIRST_VERTEX_CONVENTION #x8E4D)
  (define GL_LAST_VERTEX_CONVENTION #x8E4E)
  (define GL_PROVOKING_VERTEX #x8E4F)
  (define GL_TEXTURE_CUBE_MAP_SEAMLESS #x884F)
  (define GL_MAX_SERVER_WAIT_TIMEOUT #x9111)
  (define GL_OBJECT_TYPE #x9112)
  (define GL_SYNC_CONDITION #x9113)
  (define GL_SYNC_STATUS #x9114)
  (define GL_SYNC_FLAGS #x9115)
  (define GL_SYNC_FENCE #x9116)
  (define GL_SYNC_GPU_COMMANDS_COMPLETE #x9117)
  (define GL_UNSIGNALED #x9118)
  (define GL_SIGNALED #x9119)
  (define GL_ALREADY_SIGNALED #x911A)
  (define GL_TIMEOUT_EXPIRED #x911B)
  (define GL_CONDITION_SATISFIED #x911C)
  (define GL_WAIT_FAILED #x911D)
  (define GL_TIMEOUT_IGNORED #xFFFFFFFFFFFFFFFF)
  (define GL_SYNC_FLUSH_COMMANDS_BIT #x00000001)
  (define GL_SAMPLE_POSITION #x8E50)
  (define GL_SAMPLE_MASK #x8E51)
  (define GL_SAMPLE_MASK_VALUE #x8E52)
  (define GL_MAX_SAMPLE_MASK_WORDS #x8E59)
  (define GL_TEXTURE_2D_MULTISAMPLE #x9100)
  (define GL_PROXY_TEXTURE_2D_MULTISAMPLE #x9101)
  (define GL_TEXTURE_2D_MULTISAMPLE_ARRAY #x9102)
  (define GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY #x9103)
  (define GL_TEXTURE_BINDING_2D_MULTISAMPLE #x9104)
  (define GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY #x9105)
  (define GL_TEXTURE_SAMPLES #x9106)
  (define GL_TEXTURE_FIXED_SAMPLE_LOCATIONS #x9107)
  (define GL_SAMPLER_2D_MULTISAMPLE #x9108)
  (define GL_INT_SAMPLER_2D_MULTISAMPLE #x9109)
  (define GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE #x910A)
  (define GL_SAMPLER_2D_MULTISAMPLE_ARRAY #x910B)
  (define GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY #x910C)
  (define GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY #x910D)
  (define GL_MAX_COLOR_TEXTURE_SAMPLES #x910E)
  (define GL_MAX_DEPTH_TEXTURE_SAMPLES #x910F)
  (define GL_MAX_INTEGER_SAMPLES #x9110)
  (define GL_VERSION_3_3 1)
  (define GL_VERTEX_ATTRIB_ARRAY_DIVISOR #x88FE)
  (define GL_SRC1_COLOR #x88F9)
  (define GL_ONE_MINUS_SRC1_COLOR #x88FA)
  (define GL_ONE_MINUS_SRC1_ALPHA #x88FB)
  (define GL_MAX_DUAL_SOURCE_DRAW_BUFFERS #x88FC)
  (define GL_ANY_SAMPLES_PASSED #x8C2F)
  (define GL_SAMPLER_BINDING #x8919)
  (define GL_RGB10_A2UI #x906F)
  (define GL_TEXTURE_SWIZZLE_R #x8E42)
  (define GL_TEXTURE_SWIZZLE_G #x8E43)
  (define GL_TEXTURE_SWIZZLE_B #x8E44)
  (define GL_TEXTURE_SWIZZLE_A #x8E45)
  (define GL_TEXTURE_SWIZZLE_RGBA #x8E46)
  (define GL_TIME_ELAPSED #x88BF)
  (define GL_TIMESTAMP #x8E28)
  (define GL_INT_2_10_10_10_REV #x8D9F)
  (define GL_VERSION_4_0 1)
  (define GL_SAMPLE_SHADING #x8C36)
  (define GL_MIN_SAMPLE_SHADING_VALUE #x8C37)
  (define GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET #x8E5E)
  (define GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET #x8E5F)
  (define GL_TEXTURE_CUBE_MAP_ARRAY #x9009)
  (define GL_TEXTURE_BINDING_CUBE_MAP_ARRAY #x900A)
  (define GL_PROXY_TEXTURE_CUBE_MAP_ARRAY #x900B)
  (define GL_SAMPLER_CUBE_MAP_ARRAY #x900C)
  (define GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW #x900D)
  (define GL_INT_SAMPLER_CUBE_MAP_ARRAY #x900E)
  (define GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY #x900F)
  (define GL_DRAW_INDIRECT_BUFFER #x8F3F)
  (define GL_DRAW_INDIRECT_BUFFER_BINDING #x8F43)
  (define GL_GEOMETRY_SHADER_INVOCATIONS #x887F)
  (define GL_MAX_GEOMETRY_SHADER_INVOCATIONS #x8E5A)
  (define GL_MIN_FRAGMENT_INTERPOLATION_OFFSET #x8E5B)
  (define GL_MAX_FRAGMENT_INTERPOLATION_OFFSET #x8E5C)
  (define GL_FRAGMENT_INTERPOLATION_OFFSET_BITS #x8E5D)
  (define GL_MAX_VERTEX_STREAMS #x8E71)
  (define GL_DOUBLE_VEC2 #x8FFC)
  (define GL_DOUBLE_VEC3 #x8FFD)
  (define GL_DOUBLE_VEC4 #x8FFE)
  (define GL_DOUBLE_MAT2 #x8F46)
  (define GL_DOUBLE_MAT3 #x8F47)
  (define GL_DOUBLE_MAT4 #x8F48)
  (define GL_ACTIVE_SUBROUTINES #x8DE5)
  (define GL_ACTIVE_SUBROUTINE_UNIFORMS #x8DE6)
  (define GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS #x8E47)
  (define GL_ACTIVE_SUBROUTINE_MAX_LENGTH #x8E48)
  (define GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH #x8E49)
  (define GL_MAX_SUBROUTINES #x8DE7)
  (define GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS #x8DE8)
  (define GL_NUM_COMPATIBLE_SUBROUTINES #x8E4A)
  (define GL_COMPATIBLE_SUBROUTINES #x8E4B)
  (define GL_PATCHES #x000E)
  (define GL_PATCH_VERTICES #x8E72)
  (define GL_PATCH_DEFAULT_INNER_LEVEL #x8E73)
  (define GL_PATCH_DEFAULT_OUTER_LEVEL #x8E74)
  (define GL_TESS_CONTROL_OUTPUT_VERTICES #x8E75)
  (define GL_TESS_GEN_MODE #x8E76)
  (define GL_TESS_GEN_SPACING #x8E77)
  (define GL_TESS_GEN_VERTEX_ORDER #x8E78)
  (define GL_TESS_GEN_POINT_MODE #x8E79)
  (define GL_ISOLINES #x8E7A)
  (define GL_FRACTIONAL_ODD #x8E7B)
  (define GL_FRACTIONAL_EVEN #x8E7C)
  (define GL_MAX_PATCH_VERTICES #x8E7D)
  (define GL_MAX_TESS_GEN_LEVEL #x8E7E)
  (define GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS #x8E7F)
  (define GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS #x8E80)
  (define GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS #x8E81)
  (define GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS #x8E82)
  (define GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS #x8E83)
  (define GL_MAX_TESS_PATCH_COMPONENTS #x8E84)
  (define GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS #x8E85)
  (define GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS #x8E86)
  (define GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS #x8E89)
  (define GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS #x8E8A)
  (define GL_MAX_TESS_CONTROL_INPUT_COMPONENTS #x886C)
  (define GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS #x886D)
  (define GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS #x8E1E)
  (define GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS #x8E1F)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER #x84F0)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER #x84F1)
  (define GL_TESS_EVALUATION_SHADER #x8E87)
  (define GL_TESS_CONTROL_SHADER #x8E88)
  (define GL_TRANSFORM_FEEDBACK #x8E22)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED #x8E23)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE #x8E24)
  (define GL_TRANSFORM_FEEDBACK_BINDING #x8E25)
  (define GL_MAX_TRANSFORM_FEEDBACK_BUFFERS #x8E70)
  (define GL_VERSION_4_1 1)
  (define GL_FIXED #x140C)
  (define GL_IMPLEMENTATION_COLOR_READ_TYPE #x8B9A)
  (define GL_IMPLEMENTATION_COLOR_READ_FORMAT #x8B9B)
  (define GL_LOW_FLOAT #x8DF0)
  (define GL_MEDIUM_FLOAT #x8DF1)
  (define GL_HIGH_FLOAT #x8DF2)
  (define GL_LOW_INT #x8DF3)
  (define GL_MEDIUM_INT #x8DF4)
  (define GL_HIGH_INT #x8DF5)
  (define GL_SHADER_COMPILER #x8DFA)
  (define GL_SHADER_BINARY_FORMATS #x8DF8)
  (define GL_NUM_SHADER_BINARY_FORMATS #x8DF9)
  (define GL_MAX_VERTEX_UNIFORM_VECTORS #x8DFB)
  (define GL_MAX_VARYING_VECTORS #x8DFC)
  (define GL_MAX_FRAGMENT_UNIFORM_VECTORS #x8DFD)
  (define GL_RGB565 #x8D62)
  (define GL_PROGRAM_BINARY_RETRIEVABLE_HINT #x8257)
  (define GL_PROGRAM_BINARY_LENGTH #x8741)
  (define GL_NUM_PROGRAM_BINARY_FORMATS #x87FE)
  (define GL_PROGRAM_BINARY_FORMATS #x87FF)
  (define GL_VERTEX_SHADER_BIT #x00000001)
  (define GL_FRAGMENT_SHADER_BIT #x00000002)
  (define GL_GEOMETRY_SHADER_BIT #x00000004)
  (define GL_TESS_CONTROL_SHADER_BIT #x00000008)
  (define GL_TESS_EVALUATION_SHADER_BIT #x00000010)
  (define GL_ALL_SHADER_BITS #xFFFFFFFF)
  (define GL_PROGRAM_SEPARABLE #x8258)
  (define GL_ACTIVE_PROGRAM #x8259)
  (define GL_PROGRAM_PIPELINE_BINDING #x825A)
  (define GL_MAX_VIEWPORTS #x825B)
  (define GL_VIEWPORT_SUBPIXEL_BITS #x825C)
  (define GL_VIEWPORT_BOUNDS_RANGE #x825D)
  (define GL_LAYER_PROVOKING_VERTEX #x825E)
  (define GL_VIEWPORT_INDEX_PROVOKING_VERTEX #x825F)
  (define GL_UNDEFINED_VERTEX #x8260)
  (define GL_VERSION_4_2 1)
  (define GL_COPY_READ_BUFFER_BINDING #x8F36)
  (define GL_COPY_WRITE_BUFFER_BINDING #x8F37)
  (define GL_TRANSFORM_FEEDBACK_ACTIVE #x8E24)
  (define GL_TRANSFORM_FEEDBACK_PAUSED #x8E23)
  (define GL_UNPACK_COMPRESSED_BLOCK_WIDTH #x9127)
  (define GL_UNPACK_COMPRESSED_BLOCK_HEIGHT #x9128)
  (define GL_UNPACK_COMPRESSED_BLOCK_DEPTH #x9129)
  (define GL_UNPACK_COMPRESSED_BLOCK_SIZE #x912A)
  (define GL_PACK_COMPRESSED_BLOCK_WIDTH #x912B)
  (define GL_PACK_COMPRESSED_BLOCK_HEIGHT #x912C)
  (define GL_PACK_COMPRESSED_BLOCK_DEPTH #x912D)
  (define GL_PACK_COMPRESSED_BLOCK_SIZE #x912E)
  (define GL_NUM_SAMPLE_COUNTS #x9380)
  (define GL_MIN_MAP_BUFFER_ALIGNMENT #x90BC)
  (define GL_ATOMIC_COUNTER_BUFFER #x92C0)
  (define GL_ATOMIC_COUNTER_BUFFER_BINDING #x92C1)
  (define GL_ATOMIC_COUNTER_BUFFER_START #x92C2)
  (define GL_ATOMIC_COUNTER_BUFFER_SIZE #x92C3)
  (define GL_ATOMIC_COUNTER_BUFFER_DATA_SIZE #x92C4)
  (define GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTERS #x92C5)
  (define GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTER_INDICES #x92C6)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_VERTEX_SHADER #x92C7)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_CONTROL_SHADER #x92C8)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_EVALUATION_SHADER #x92C9)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_GEOMETRY_SHADER #x92CA)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_FRAGMENT_SHADER #x92CB)
  (define GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS #x92CC)
  (define GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS #x92CD)
  (define GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS #x92CE)
  (define GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS #x92CF)
  (define GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS #x92D0)
  (define GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS #x92D1)
  (define GL_MAX_VERTEX_ATOMIC_COUNTERS #x92D2)
  (define GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS #x92D3)
  (define GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS #x92D4)
  (define GL_MAX_GEOMETRY_ATOMIC_COUNTERS #x92D5)
  (define GL_MAX_FRAGMENT_ATOMIC_COUNTERS #x92D6)
  (define GL_MAX_COMBINED_ATOMIC_COUNTERS #x92D7)
  (define GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE #x92D8)
  (define GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS #x92DC)
  (define GL_ACTIVE_ATOMIC_COUNTER_BUFFERS #x92D9)
  (define GL_UNIFORM_ATOMIC_COUNTER_BUFFER_INDEX #x92DA)
  (define GL_UNSIGNED_INT_ATOMIC_COUNTER #x92DB)
  (define GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT #x00000001)
  (define GL_ELEMENT_ARRAY_BARRIER_BIT #x00000002)
  (define GL_UNIFORM_BARRIER_BIT #x00000004)
  (define GL_TEXTURE_FETCH_BARRIER_BIT #x00000008)
  (define GL_SHADER_IMAGE_ACCESS_BARRIER_BIT #x00000020)
  (define GL_COMMAND_BARRIER_BIT #x00000040)
  (define GL_PIXEL_BUFFER_BARRIER_BIT #x00000080)
  (define GL_TEXTURE_UPDATE_BARRIER_BIT #x00000100)
  (define GL_BUFFER_UPDATE_BARRIER_BIT #x00000200)
  (define GL_FRAMEBUFFER_BARRIER_BIT #x00000400)
  (define GL_TRANSFORM_FEEDBACK_BARRIER_BIT #x00000800)
  (define GL_ATOMIC_COUNTER_BARRIER_BIT #x00001000)
  (define GL_ALL_BARRIER_BITS #xFFFFFFFF)
  (define GL_MAX_IMAGE_UNITS #x8F38)
  (define GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS #x8F39)
  (define GL_IMAGE_BINDING_NAME #x8F3A)
  (define GL_IMAGE_BINDING_LEVEL #x8F3B)
  (define GL_IMAGE_BINDING_LAYERED #x8F3C)
  (define GL_IMAGE_BINDING_LAYER #x8F3D)
  (define GL_IMAGE_BINDING_ACCESS #x8F3E)
  (define GL_IMAGE_1D #x904C)
  (define GL_IMAGE_2D #x904D)
  (define GL_IMAGE_3D #x904E)
  (define GL_IMAGE_2D_RECT #x904F)
  (define GL_IMAGE_CUBE #x9050)
  (define GL_IMAGE_BUFFER #x9051)
  (define GL_IMAGE_1D_ARRAY #x9052)
  (define GL_IMAGE_2D_ARRAY #x9053)
  (define GL_IMAGE_CUBE_MAP_ARRAY #x9054)
  (define GL_IMAGE_2D_MULTISAMPLE #x9055)
  (define GL_IMAGE_2D_MULTISAMPLE_ARRAY #x9056)
  (define GL_INT_IMAGE_1D #x9057)
  (define GL_INT_IMAGE_2D #x9058)
  (define GL_INT_IMAGE_3D #x9059)
  (define GL_INT_IMAGE_2D_RECT #x905A)
  (define GL_INT_IMAGE_CUBE #x905B)
  (define GL_INT_IMAGE_BUFFER #x905C)
  (define GL_INT_IMAGE_1D_ARRAY #x905D)
  (define GL_INT_IMAGE_2D_ARRAY #x905E)
  (define GL_INT_IMAGE_CUBE_MAP_ARRAY #x905F)
  (define GL_INT_IMAGE_2D_MULTISAMPLE #x9060)
  (define GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY #x9061)
  (define GL_UNSIGNED_INT_IMAGE_1D #x9062)
  (define GL_UNSIGNED_INT_IMAGE_2D #x9063)
  (define GL_UNSIGNED_INT_IMAGE_3D #x9064)
  (define GL_UNSIGNED_INT_IMAGE_2D_RECT #x9065)
  (define GL_UNSIGNED_INT_IMAGE_CUBE #x9066)
  (define GL_UNSIGNED_INT_IMAGE_BUFFER #x9067)
  (define GL_UNSIGNED_INT_IMAGE_1D_ARRAY #x9068)
  (define GL_UNSIGNED_INT_IMAGE_2D_ARRAY #x9069)
  (define GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY #x906A)
  (define GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE #x906B)
  (define GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY #x906C)
  (define GL_MAX_IMAGE_SAMPLES #x906D)
  (define GL_IMAGE_BINDING_FORMAT #x906E)
  (define GL_IMAGE_FORMAT_COMPATIBILITY_TYPE #x90C7)
  (define GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE #x90C8)
  (define GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS #x90C9)
  (define GL_MAX_VERTEX_IMAGE_UNIFORMS #x90CA)
  (define GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS #x90CB)
  (define GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS #x90CC)
  (define GL_MAX_GEOMETRY_IMAGE_UNIFORMS #x90CD)
  (define GL_MAX_FRAGMENT_IMAGE_UNIFORMS #x90CE)
  (define GL_MAX_COMBINED_IMAGE_UNIFORMS #x90CF)
  (define GL_COMPRESSED_RGBA_BPTC_UNORM #x8E8C)
  (define GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM #x8E8D)
  (define GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT #x8E8E)
  (define GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT #x8E8F)
  (define GL_TEXTURE_IMMUTABLE_FORMAT #x912F)
  (define GL_VERSION_4_3 1)
  (define GL_NUM_SHADING_LANGUAGE_VERSIONS #x82E9)
  (define GL_VERTEX_ATTRIB_ARRAY_LONG #x874E)
  (define GL_COMPRESSED_RGB8_ETC2 #x9274)
  (define GL_COMPRESSED_SRGB8_ETC2 #x9275)
  (define GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 #x9276)
  (define GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 #x9277)
  (define GL_COMPRESSED_RGBA8_ETC2_EAC #x9278)
  (define GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC #x9279)
  (define GL_COMPRESSED_R11_EAC #x9270)
  (define GL_COMPRESSED_SIGNED_R11_EAC #x9271)
  (define GL_COMPRESSED_RG11_EAC #x9272)
  (define GL_COMPRESSED_SIGNED_RG11_EAC #x9273)
  (define GL_PRIMITIVE_RESTART_FIXED_INDEX #x8D69)
  (define GL_ANY_SAMPLES_PASSED_CONSERVATIVE #x8D6A)
  (define GL_MAX_ELEMENT_INDEX #x8D6B)
  (define GL_COMPUTE_SHADER #x91B9)
  (define GL_MAX_COMPUTE_UNIFORM_BLOCKS #x91BB)
  (define GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS #x91BC)
  (define GL_MAX_COMPUTE_IMAGE_UNIFORMS #x91BD)
  (define GL_MAX_COMPUTE_SHARED_MEMORY_SIZE #x8262)
  (define GL_MAX_COMPUTE_UNIFORM_COMPONENTS #x8263)
  (define GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS #x8264)
  (define GL_MAX_COMPUTE_ATOMIC_COUNTERS #x8265)
  (define GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS #x8266)
  (define GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS #x90EB)
  (define GL_MAX_COMPUTE_WORK_GROUP_COUNT #x91BE)
  (define GL_MAX_COMPUTE_WORK_GROUP_SIZE #x91BF)
  (define GL_COMPUTE_WORK_GROUP_SIZE #x8267)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER #x90EC)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_COMPUTE_SHADER #x90ED)
  (define GL_DISPATCH_INDIRECT_BUFFER #x90EE)
  (define GL_DISPATCH_INDIRECT_BUFFER_BINDING #x90EF)
  (define GL_COMPUTE_SHADER_BIT #x00000020)
  (define GL_DEBUG_OUTPUT_SYNCHRONOUS #x8242)
  (define GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH #x8243)
  (define GL_DEBUG_CALLBACK_FUNCTION #x8244)
  (define GL_DEBUG_CALLBACK_USER_PARAM #x8245)
  (define GL_DEBUG_SOURCE_API #x8246)
  (define GL_DEBUG_SOURCE_WINDOW_SYSTEM #x8247)
  (define GL_DEBUG_SOURCE_SHADER_COMPILER #x8248)
  (define GL_DEBUG_SOURCE_THIRD_PARTY #x8249)
  (define GL_DEBUG_SOURCE_APPLICATION #x824A)
  (define GL_DEBUG_SOURCE_OTHER #x824B)
  (define GL_DEBUG_TYPE_ERROR #x824C)
  (define GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR #x824D)
  (define GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR #x824E)
  (define GL_DEBUG_TYPE_PORTABILITY #x824F)
  (define GL_DEBUG_TYPE_PERFORMANCE #x8250)
  (define GL_DEBUG_TYPE_OTHER #x8251)
  (define GL_MAX_DEBUG_MESSAGE_LENGTH #x9143)
  (define GL_MAX_DEBUG_LOGGED_MESSAGES #x9144)
  (define GL_DEBUG_LOGGED_MESSAGES #x9145)
  (define GL_DEBUG_SEVERITY_HIGH #x9146)
  (define GL_DEBUG_SEVERITY_MEDIUM #x9147)
  (define GL_DEBUG_SEVERITY_LOW #x9148)
  (define GL_DEBUG_TYPE_MARKER #x8268)
  (define GL_DEBUG_TYPE_PUSH_GROUP #x8269)
  (define GL_DEBUG_TYPE_POP_GROUP #x826A)
  (define GL_DEBUG_SEVERITY_NOTIFICATION #x826B)
  (define GL_MAX_DEBUG_GROUP_STACK_DEPTH #x826C)
  (define GL_DEBUG_GROUP_STACK_DEPTH #x826D)
  (define GL_BUFFER #x82E0)
  (define GL_SHADER #x82E1)
  (define GL_PROGRAM #x82E2)
  (define GL_QUERY #x82E3)
  (define GL_PROGRAM_PIPELINE #x82E4)
  (define GL_SAMPLER #x82E6)
  (define GL_MAX_LABEL_LENGTH #x82E8)
  (define GL_DEBUG_OUTPUT #x92E0)
  (define GL_CONTEXT_FLAG_DEBUG_BIT #x00000002)
  (define GL_MAX_UNIFORM_LOCATIONS #x826E)
  (define GL_FRAMEBUFFER_DEFAULT_WIDTH #x9310)
  (define GL_FRAMEBUFFER_DEFAULT_HEIGHT #x9311)
  (define GL_FRAMEBUFFER_DEFAULT_LAYERS #x9312)
  (define GL_FRAMEBUFFER_DEFAULT_SAMPLES #x9313)
  (define GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS #x9314)
  (define GL_MAX_FRAMEBUFFER_WIDTH #x9315)
  (define GL_MAX_FRAMEBUFFER_HEIGHT #x9316)
  (define GL_MAX_FRAMEBUFFER_LAYERS #x9317)
  (define GL_MAX_FRAMEBUFFER_SAMPLES #x9318)
  (define GL_INTERNALFORMAT_SUPPORTED #x826F)
  (define GL_INTERNALFORMAT_PREFERRED #x8270)
  (define GL_INTERNALFORMAT_RED_SIZE #x8271)
  (define GL_INTERNALFORMAT_GREEN_SIZE #x8272)
  (define GL_INTERNALFORMAT_BLUE_SIZE #x8273)
  (define GL_INTERNALFORMAT_ALPHA_SIZE #x8274)
  (define GL_INTERNALFORMAT_DEPTH_SIZE #x8275)
  (define GL_INTERNALFORMAT_STENCIL_SIZE #x8276)
  (define GL_INTERNALFORMAT_SHARED_SIZE #x8277)
  (define GL_INTERNALFORMAT_RED_TYPE #x8278)
  (define GL_INTERNALFORMAT_GREEN_TYPE #x8279)
  (define GL_INTERNALFORMAT_BLUE_TYPE #x827A)
  (define GL_INTERNALFORMAT_ALPHA_TYPE #x827B)
  (define GL_INTERNALFORMAT_DEPTH_TYPE #x827C)
  (define GL_INTERNALFORMAT_STENCIL_TYPE #x827D)
  (define GL_MAX_WIDTH #x827E)
  (define GL_MAX_HEIGHT #x827F)
  (define GL_MAX_DEPTH #x8280)
  (define GL_MAX_LAYERS #x8281)
  (define GL_MAX_COMBINED_DIMENSIONS #x8282)
  (define GL_COLOR_COMPONENTS #x8283)
  (define GL_DEPTH_COMPONENTS #x8284)
  (define GL_STENCIL_COMPONENTS #x8285)
  (define GL_COLOR_RENDERABLE #x8286)
  (define GL_DEPTH_RENDERABLE #x8287)
  (define GL_STENCIL_RENDERABLE #x8288)
  (define GL_FRAMEBUFFER_RENDERABLE #x8289)
  (define GL_FRAMEBUFFER_RENDERABLE_LAYERED #x828A)
  (define GL_FRAMEBUFFER_BLEND #x828B)
  (define GL_READ_PIXELS #x828C)
  (define GL_READ_PIXELS_FORMAT #x828D)
  (define GL_READ_PIXELS_TYPE #x828E)
  (define GL_TEXTURE_IMAGE_FORMAT #x828F)
  (define GL_TEXTURE_IMAGE_TYPE #x8290)
  (define GL_GET_TEXTURE_IMAGE_FORMAT #x8291)
  (define GL_GET_TEXTURE_IMAGE_TYPE #x8292)
  (define GL_MIPMAP #x8293)
  (define GL_MANUAL_GENERATE_MIPMAP #x8294)
  (define GL_AUTO_GENERATE_MIPMAP #x8295)
  (define GL_COLOR_ENCODING #x8296)
  (define GL_SRGB_READ #x8297)
  (define GL_SRGB_WRITE #x8298)
  (define GL_FILTER #x829A)
  (define GL_VERTEX_TEXTURE #x829B)
  (define GL_TESS_CONTROL_TEXTURE #x829C)
  (define GL_TESS_EVALUATION_TEXTURE #x829D)
  (define GL_GEOMETRY_TEXTURE #x829E)
  (define GL_FRAGMENT_TEXTURE #x829F)
  (define GL_COMPUTE_TEXTURE #x82A0)
  (define GL_TEXTURE_SHADOW #x82A1)
  (define GL_TEXTURE_GATHER #x82A2)
  (define GL_TEXTURE_GATHER_SHADOW #x82A3)
  (define GL_SHADER_IMAGE_LOAD #x82A4)
  (define GL_SHADER_IMAGE_STORE #x82A5)
  (define GL_SHADER_IMAGE_ATOMIC #x82A6)
  (define GL_IMAGE_TEXEL_SIZE #x82A7)
  (define GL_IMAGE_COMPATIBILITY_CLASS #x82A8)
  (define GL_IMAGE_PIXEL_FORMAT #x82A9)
  (define GL_IMAGE_PIXEL_TYPE #x82AA)
  (define GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_TEST #x82AC)
  (define GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_TEST #x82AD)
  (define GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_WRITE #x82AE)
  (define GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_WRITE #x82AF)
  (define GL_TEXTURE_COMPRESSED_BLOCK_WIDTH #x82B1)
  (define GL_TEXTURE_COMPRESSED_BLOCK_HEIGHT #x82B2)
  (define GL_TEXTURE_COMPRESSED_BLOCK_SIZE #x82B3)
  (define GL_CLEAR_BUFFER #x82B4)
  (define GL_TEXTURE_VIEW #x82B5)
  (define GL_VIEW_COMPATIBILITY_CLASS #x82B6)
  (define GL_FULL_SUPPORT #x82B7)
  (define GL_CAVEAT_SUPPORT #x82B8)
  (define GL_IMAGE_CLASS_4_X_32 #x82B9)
  (define GL_IMAGE_CLASS_2_X_32 #x82BA)
  (define GL_IMAGE_CLASS_1_X_32 #x82BB)
  (define GL_IMAGE_CLASS_4_X_16 #x82BC)
  (define GL_IMAGE_CLASS_2_X_16 #x82BD)
  (define GL_IMAGE_CLASS_1_X_16 #x82BE)
  (define GL_IMAGE_CLASS_4_X_8 #x82BF)
  (define GL_IMAGE_CLASS_2_X_8 #x82C0)
  (define GL_IMAGE_CLASS_1_X_8 #x82C1)
  (define GL_IMAGE_CLASS_11_11_10 #x82C2)
  (define GL_IMAGE_CLASS_10_10_10_2 #x82C3)
  (define GL_VIEW_CLASS_128_BITS #x82C4)
  (define GL_VIEW_CLASS_96_BITS #x82C5)
  (define GL_VIEW_CLASS_64_BITS #x82C6)
  (define GL_VIEW_CLASS_48_BITS #x82C7)
  (define GL_VIEW_CLASS_32_BITS #x82C8)
  (define GL_VIEW_CLASS_24_BITS #x82C9)
  (define GL_VIEW_CLASS_16_BITS #x82CA)
  (define GL_VIEW_CLASS_8_BITS #x82CB)
  (define GL_VIEW_CLASS_S3TC_DXT1_RGB #x82CC)
  (define GL_VIEW_CLASS_S3TC_DXT1_RGBA #x82CD)
  (define GL_VIEW_CLASS_S3TC_DXT3_RGBA #x82CE)
  (define GL_VIEW_CLASS_S3TC_DXT5_RGBA #x82CF)
  (define GL_VIEW_CLASS_RGTC1_RED #x82D0)
  (define GL_VIEW_CLASS_RGTC2_RG #x82D1)
  (define GL_VIEW_CLASS_BPTC_UNORM #x82D2)
  (define GL_VIEW_CLASS_BPTC_FLOAT #x82D3)
  (define GL_UNIFORM #x92E1)
  (define GL_UNIFORM_BLOCK #x92E2)
  (define GL_PROGRAM_INPUT #x92E3)
  (define GL_PROGRAM_OUTPUT #x92E4)
  (define GL_BUFFER_VARIABLE #x92E5)
  (define GL_SHADER_STORAGE_BLOCK #x92E6)
  (define GL_VERTEX_SUBROUTINE #x92E8)
  (define GL_TESS_CONTROL_SUBROUTINE #x92E9)
  (define GL_TESS_EVALUATION_SUBROUTINE #x92EA)
  (define GL_GEOMETRY_SUBROUTINE #x92EB)
  (define GL_FRAGMENT_SUBROUTINE #x92EC)
  (define GL_COMPUTE_SUBROUTINE #x92ED)
  (define GL_VERTEX_SUBROUTINE_UNIFORM #x92EE)
  (define GL_TESS_CONTROL_SUBROUTINE_UNIFORM #x92EF)
  (define GL_TESS_EVALUATION_SUBROUTINE_UNIFORM #x92F0)
  (define GL_GEOMETRY_SUBROUTINE_UNIFORM #x92F1)
  (define GL_FRAGMENT_SUBROUTINE_UNIFORM #x92F2)
  (define GL_COMPUTE_SUBROUTINE_UNIFORM #x92F3)
  (define GL_TRANSFORM_FEEDBACK_VARYING #x92F4)
  (define GL_ACTIVE_RESOURCES #x92F5)
  (define GL_MAX_NAME_LENGTH #x92F6)
  (define GL_MAX_NUM_ACTIVE_VARIABLES #x92F7)
  (define GL_MAX_NUM_COMPATIBLE_SUBROUTINES #x92F8)
  (define GL_NAME_LENGTH #x92F9)
  (define GL_TYPE #x92FA)
  (define GL_ARRAY_SIZE #x92FB)
  (define GL_OFFSET #x92FC)
  (define GL_BLOCK_INDEX #x92FD)
  (define GL_ARRAY_STRIDE #x92FE)
  (define GL_MATRIX_STRIDE #x92FF)
  (define GL_IS_ROW_MAJOR #x9300)
  (define GL_ATOMIC_COUNTER_BUFFER_INDEX #x9301)
  (define GL_BUFFER_BINDING #x9302)
  (define GL_BUFFER_DATA_SIZE #x9303)
  (define GL_NUM_ACTIVE_VARIABLES #x9304)
  (define GL_ACTIVE_VARIABLES #x9305)
  (define GL_REFERENCED_BY_VERTEX_SHADER #x9306)
  (define GL_REFERENCED_BY_TESS_CONTROL_SHADER #x9307)
  (define GL_REFERENCED_BY_TESS_EVALUATION_SHADER #x9308)
  (define GL_REFERENCED_BY_GEOMETRY_SHADER #x9309)
  (define GL_REFERENCED_BY_FRAGMENT_SHADER #x930A)
  (define GL_REFERENCED_BY_COMPUTE_SHADER #x930B)
  (define GL_TOP_LEVEL_ARRAY_SIZE #x930C)
  (define GL_TOP_LEVEL_ARRAY_STRIDE #x930D)
  (define GL_LOCATION #x930E)
  (define GL_LOCATION_INDEX #x930F)
  (define GL_IS_PER_PATCH #x92E7)
  (define GL_SHADER_STORAGE_BUFFER #x90D2)
  (define GL_SHADER_STORAGE_BUFFER_BINDING #x90D3)
  (define GL_SHADER_STORAGE_BUFFER_START #x90D4)
  (define GL_SHADER_STORAGE_BUFFER_SIZE #x90D5)
  (define GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS #x90D6)
  (define GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS #x90D7)
  (define GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS #x90D8)
  (define GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS #x90D9)
  (define GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS #x90DA)
  (define GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS #x90DB)
  (define GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS #x90DC)
  (define GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS #x90DD)
  (define GL_MAX_SHADER_STORAGE_BLOCK_SIZE #x90DE)
  (define GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT #x90DF)
  (define GL_SHADER_STORAGE_BARRIER_BIT #x00002000)
  (define GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES #x8F39)
  (define GL_DEPTH_STENCIL_TEXTURE_MODE #x90EA)
  (define GL_TEXTURE_BUFFER_OFFSET #x919D)
  (define GL_TEXTURE_BUFFER_SIZE #x919E)
  (define GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT #x919F)
  (define GL_TEXTURE_VIEW_MIN_LEVEL #x82DB)
  (define GL_TEXTURE_VIEW_NUM_LEVELS #x82DC)
  (define GL_TEXTURE_VIEW_MIN_LAYER #x82DD)
  (define GL_TEXTURE_VIEW_NUM_LAYERS #x82DE)
  (define GL_TEXTURE_IMMUTABLE_LEVELS #x82DF)
  (define GL_VERTEX_ATTRIB_BINDING #x82D4)
  (define GL_VERTEX_ATTRIB_RELATIVE_OFFSET #x82D5)
  (define GL_VERTEX_BINDING_DIVISOR #x82D6)
  (define GL_VERTEX_BINDING_OFFSET #x82D7)
  (define GL_VERTEX_BINDING_STRIDE #x82D8)
  (define GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET #x82D9)
  (define GL_MAX_VERTEX_ATTRIB_BINDINGS #x82DA)
  (define GL_VERTEX_BINDING_BUFFER #x8F4F)
  (define GL_VERSION_4_4 1)
  (define GL_MAX_VERTEX_ATTRIB_STRIDE #x82E5)
  (define GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED #x8221)
  (define GL_TEXTURE_BUFFER_BINDING #x8C2A)
  (define GL_MAP_PERSISTENT_BIT #x0040)
  (define GL_MAP_COHERENT_BIT #x0080)
  (define GL_DYNAMIC_STORAGE_BIT #x0100)
  (define GL_CLIENT_STORAGE_BIT #x0200)
  (define GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT #x00004000)
  (define GL_BUFFER_IMMUTABLE_STORAGE #x821F)
  (define GL_BUFFER_STORAGE_FLAGS #x8220)
  (define GL_CLEAR_TEXTURE #x9365)
  (define GL_LOCATION_COMPONENT #x934A)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_INDEX #x934B)
  (define GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE #x934C)
  (define GL_QUERY_BUFFER #x9192)
  (define GL_QUERY_BUFFER_BARRIER_BIT #x00008000)
  (define GL_QUERY_BUFFER_BINDING #x9193)
  (define GL_QUERY_RESULT_NO_WAIT #x9194)
  (define GL_MIRROR_CLAMP_TO_EDGE #x8743)
  (define GL_VERSION_4_5 1)
  (define GL_CONTEXT_LOST #x0507)
  (define GL_NEGATIVE_ONE_TO_ONE #x935E)
  (define GL_ZERO_TO_ONE #x935F)
  (define GL_CLIP_ORIGIN #x935C)
  (define GL_CLIP_DEPTH_MODE #x935D)
  (define GL_QUERY_WAIT_INVERTED #x8E17)
  (define GL_QUERY_NO_WAIT_INVERTED #x8E18)
  (define GL_QUERY_BY_REGION_WAIT_INVERTED #x8E19)
  (define GL_QUERY_BY_REGION_NO_WAIT_INVERTED #x8E1A)
  (define GL_MAX_CULL_DISTANCES #x82F9)
  (define GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES #x82FA)
  (define GL_TEXTURE_TARGET #x1006)
  (define GL_QUERY_TARGET #x82EA)
  (define GL_GUILTY_CONTEXT_RESET #x8253)
  (define GL_INNOCENT_CONTEXT_RESET #x8254)
  (define GL_UNKNOWN_CONTEXT_RESET #x8255)
  (define GL_RESET_NOTIFICATION_STRATEGY #x8256)
  (define GL_LOSE_CONTEXT_ON_RESET #x8252)
  (define GL_NO_RESET_NOTIFICATION #x8261)
  (define GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT #x00000004)
  (define GL_CONTEXT_RELEASE_BEHAVIOR #x82FB)
  (define GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH #x82FC)
  (define GL_VERSION_4_6 1)
  (define GL_SHADER_BINARY_FORMAT_SPIR_V #x9551)
  (define GL_SPIR_V_BINARY #x9552)
  (define GL_PARAMETER_BUFFER #x80EE)
  (define GL_PARAMETER_BUFFER_BINDING #x80EF)
  (define GL_CONTEXT_FLAG_NO_ERROR_BIT #x00000008)
  (define GL_VERTICES_SUBMITTED #x82EE)
  (define GL_PRIMITIVES_SUBMITTED #x82EF)
  (define GL_VERTEX_SHADER_INVOCATIONS #x82F0)
  (define GL_TESS_CONTROL_SHADER_PATCHES #x82F1)
  (define GL_TESS_EVALUATION_SHADER_INVOCATIONS #x82F2)
  (define GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED #x82F3)
  (define GL_FRAGMENT_SHADER_INVOCATIONS #x82F4)
  (define GL_COMPUTE_SHADER_INVOCATIONS #x82F5)
  (define GL_CLIPPING_INPUT_PRIMITIVES #x82F6)
  (define GL_CLIPPING_OUTPUT_PRIMITIVES #x82F7)
  (define GL_POLYGON_OFFSET_CLAMP #x8E1B)
  (define GL_SPIR_V_EXTENSIONS #x9553)
  (define GL_NUM_SPIR_V_EXTENSIONS #x9554)
  (define GL_TEXTURE_MAX_ANISOTROPY #x84FE)
  (define GL_MAX_TEXTURE_MAX_ANISOTROPY #x84FF)
  (define GL_TRANSFORM_FEEDBACK_OVERFLOW #x82EC)
  (define GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW #x82ED)
  (define GL_PRIMITIVE_BOUNDING_BOX_ARB #x92BE)
  (define GL_MULTISAMPLE_LINE_WIDTH_RANGE_ARB #x9381)
  (define GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY_ARB #x9382)
  (define GL_UNSIGNED_INT64_ARB #x140F)
  (define GL_SYNC_CL_EVENT_ARB #x8240)
  (define GL_SYNC_CL_EVENT_COMPLETE_ARB #x8241)
  (define GL_MAX_COMPUTE_VARIABLE_GROUP_INVOCATIONS_ARB #x9344)
  (define GL_MAX_COMPUTE_FIXED_GROUP_INVOCATIONS_ARB #x90EB)
  (define GL_MAX_COMPUTE_VARIABLE_GROUP_SIZE_ARB #x9345)
  (define GL_MAX_COMPUTE_FIXED_GROUP_SIZE_ARB #x91BF)
  (define GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB #x8242)
  (define GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB #x8243)
  (define GL_DEBUG_CALLBACK_FUNCTION_ARB #x8244)
  (define GL_DEBUG_CALLBACK_USER_PARAM_ARB #x8245)
  (define GL_DEBUG_SOURCE_API_ARB #x8246)
  (define GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB #x8247)
  (define GL_DEBUG_SOURCE_SHADER_COMPILER_ARB #x8248)
  (define GL_DEBUG_SOURCE_THIRD_PARTY_ARB #x8249)
  (define GL_DEBUG_SOURCE_APPLICATION_ARB #x824A)
  (define GL_DEBUG_SOURCE_OTHER_ARB #x824B)
  (define GL_DEBUG_TYPE_ERROR_ARB #x824C)
  (define GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB #x824D)
  (define GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB #x824E)
  (define GL_DEBUG_TYPE_PORTABILITY_ARB #x824F)
  (define GL_DEBUG_TYPE_PERFORMANCE_ARB #x8250)
  (define GL_DEBUG_TYPE_OTHER_ARB #x8251)
  (define GL_MAX_DEBUG_MESSAGE_LENGTH_ARB #x9143)
  (define GL_MAX_DEBUG_LOGGED_MESSAGES_ARB #x9144)
  (define GL_DEBUG_LOGGED_MESSAGES_ARB #x9145)
  (define GL_DEBUG_SEVERITY_HIGH_ARB #x9146)
  (define GL_DEBUG_SEVERITY_MEDIUM_ARB #x9147)
  (define GL_DEBUG_SEVERITY_LOW_ARB #x9148)
  (define GL_LINES_ADJACENCY_ARB #x000A)
  (define GL_LINE_STRIP_ADJACENCY_ARB #x000B)
  (define GL_TRIANGLES_ADJACENCY_ARB #x000C)
  (define GL_TRIANGLE_STRIP_ADJACENCY_ARB #x000D)
  (define GL_PROGRAM_POINT_SIZE_ARB #x8642)
  (define GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB #x8C29)
  (define GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB #x8DA7)
  (define GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB #x8DA8)
  (define GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB #x8DA9)
  (define GL_GEOMETRY_SHADER_ARB #x8DD9)
  (define GL_GEOMETRY_VERTICES_OUT_ARB #x8DDA)
  (define GL_GEOMETRY_INPUT_TYPE_ARB #x8DDB)
  (define GL_GEOMETRY_OUTPUT_TYPE_ARB #x8DDC)
  (define GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB #x8DDD)
  (define GL_MAX_VERTEX_VARYING_COMPONENTS_ARB #x8DDE)
  (define GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB #x8DDF)
  (define GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB #x8DE0)
  (define GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB #x8DE1)
  (define GL_SHADER_BINARY_FORMAT_SPIR_V_ARB #x9551)
  (define GL_SPIR_V_BINARY_ARB #x9552)
  (define GL_INT64_ARB #x140E)
  (define GL_INT64_VEC2_ARB #x8FE9)
  (define GL_INT64_VEC3_ARB #x8FEA)
  (define GL_INT64_VEC4_ARB #x8FEB)
  (define GL_UNSIGNED_INT64_VEC2_ARB #x8FF5)
  (define GL_UNSIGNED_INT64_VEC3_ARB #x8FF6)
  (define GL_UNSIGNED_INT64_VEC4_ARB #x8FF7)
  (define GL_PARAMETER_BUFFER_ARB #x80EE)
  (define GL_PARAMETER_BUFFER_BINDING_ARB #x80EF)
  (define GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB #x88FE)
  (define GL_SRGB_DECODE_ARB #x8299)
  (define GL_VIEW_CLASS_EAC_R11 #x9383)
  (define GL_VIEW_CLASS_EAC_RG11 #x9384)
  (define GL_VIEW_CLASS_ETC2_RGB #x9385)
  (define GL_VIEW_CLASS_ETC2_RGBA #x9386)
  (define GL_VIEW_CLASS_ETC2_EAC_RGBA #x9387)
  (define GL_MAX_SHADER_COMPILER_THREADS_ARB #x91B0)
  (define GL_COMPLETION_STATUS_ARB #x91B1)
  (define GL_VERTICES_SUBMITTED_ARB #x82EE)
  (define GL_PRIMITIVES_SUBMITTED_ARB #x82EF)
  (define GL_VERTEX_SHADER_INVOCATIONS_ARB #x82F0)
  (define GL_TESS_CONTROL_SHADER_PATCHES_ARB #x82F1)
  (define GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB #x82F2)
  (define GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB #x82F3)
  (define GL_FRAGMENT_SHADER_INVOCATIONS_ARB #x82F4)
  (define GL_COMPUTE_SHADER_INVOCATIONS_ARB #x82F5)
  (define GL_CLIPPING_INPUT_PRIMITIVES_ARB #x82F6)
  (define GL_CLIPPING_OUTPUT_PRIMITIVES_ARB #x82F7)
  (define GL_PIXEL_PACK_BUFFER_ARB #x88EB)
  (define GL_PIXEL_UNPACK_BUFFER_ARB #x88EC)
  (define GL_PIXEL_PACK_BUFFER_BINDING_ARB #x88ED)
  (define GL_PIXEL_UNPACK_BUFFER_BINDING_ARB #x88EF)
  (define GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB #x00000004)
  (define GL_LOSE_CONTEXT_ON_RESET_ARB #x8252)
  (define GL_GUILTY_CONTEXT_RESET_ARB #x8253)
  (define GL_INNOCENT_CONTEXT_RESET_ARB #x8254)
  (define GL_UNKNOWN_CONTEXT_RESET_ARB #x8255)
  (define GL_RESET_NOTIFICATION_STRATEGY_ARB #x8256)
  (define GL_NO_RESET_NOTIFICATION_ARB #x8261)
  (define GL_SAMPLE_LOCATION_SUBPIXEL_BITS_ARB #x933D)
  (define GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_ARB #x933E)
  (define GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_ARB #x933F)
  (define GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_ARB #x9340)
  (define GL_SAMPLE_LOCATION_ARB #x8E50)
  (define GL_PROGRAMMABLE_SAMPLE_LOCATION_ARB #x9341)
  (define GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_ARB #x9342)
  (define GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_ARB #x9343)
  (define GL_SAMPLE_SHADING_ARB #x8C36)
  (define GL_MIN_SAMPLE_SHADING_VALUE_ARB #x8C37)
  (define GL_SHADER_INCLUDE_ARB #x8DAE)
  (define GL_NAMED_STRING_LENGTH_ARB #x8DE9)
  (define GL_NAMED_STRING_TYPE_ARB #x8DEA)
  (define GL_SPARSE_STORAGE_BIT_ARB #x0400)
  (define GL_SPARSE_BUFFER_PAGE_SIZE_ARB #x82F8)
  (define GL_TEXTURE_SPARSE_ARB #x91A6)
  (define GL_VIRTUAL_PAGE_SIZE_INDEX_ARB #x91A7)
  (define GL_NUM_SPARSE_LEVELS_ARB #x91AA)
  (define GL_NUM_VIRTUAL_PAGE_SIZES_ARB #x91A8)
  (define GL_VIRTUAL_PAGE_SIZE_X_ARB #x9195)
  (define GL_VIRTUAL_PAGE_SIZE_Y_ARB #x9196)
  (define GL_VIRTUAL_PAGE_SIZE_Z_ARB #x9197)
  (define GL_MAX_SPARSE_TEXTURE_SIZE_ARB #x9198)
  (define GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB #x9199)
  (define GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB #x919A)
  (define GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_ARB #x91A9)
  (define GL_CLAMP_TO_BORDER_ARB #x812D)
  (define GL_TEXTURE_BUFFER_ARB #x8C2A)
  (define GL_MAX_TEXTURE_BUFFER_SIZE_ARB #x8C2B)
  (define GL_TEXTURE_BINDING_BUFFER_ARB #x8C2C)
  (define GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB #x8C2D)
  (define GL_TEXTURE_BUFFER_FORMAT_ARB #x8C2E)
  (define GL_COMPRESSED_RGBA_BPTC_UNORM_ARB #x8E8C)
  (define GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB #x8E8D)
  (define GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB #x8E8E)
  (define GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB #x8E8F)
  (define GL_TEXTURE_CUBE_MAP_ARRAY_ARB #x9009)
  (define GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB #x900A)
  (define GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB #x900B)
  (define GL_SAMPLER_CUBE_MAP_ARRAY_ARB #x900C)
  (define GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB #x900D)
  (define GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB #x900E)
  (define GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB #x900F)
  (define GL_TEXTURE_REDUCTION_MODE_ARB #x9366)
  (define GL_WEIGHTED_AVERAGE_ARB #x9367)
  (define GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB #x8E5E)
  (define GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB #x8E5F)
  (define GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB #x8F9F)
  (define GL_MIRRORED_REPEAT_ARB #x8370)
  (define GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB #x82EC)
  (define GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB #x82ED)
  (define GL_MULTIPLY_KHR #x9294)
  (define GL_SCREEN_KHR #x9295)
  (define GL_OVERLAY_KHR #x9296)
  (define GL_DARKEN_KHR #x9297)
  (define GL_LIGHTEN_KHR #x9298)
  (define GL_COLORDODGE_KHR #x9299)
  (define GL_COLORBURN_KHR #x929A)
  (define GL_HARDLIGHT_KHR #x929B)
  (define GL_SOFTLIGHT_KHR #x929C)
  (define GL_DIFFERENCE_KHR #x929E)
  (define GL_EXCLUSION_KHR #x92A0)
  (define GL_HSL_HUE_KHR #x92AD)
  (define GL_HSL_SATURATION_KHR #x92AE)
  (define GL_HSL_COLOR_KHR #x92AF)
  (define GL_HSL_LUMINOSITY_KHR #x92B0)
  (define GL_BLEND_ADVANCED_COHERENT_KHR #x9285)
  (define GL_CONTEXT_FLAG_NO_ERROR_BIT_KHR #x00000008)
  (define GL_MAX_SHADER_COMPILER_THREADS_KHR #x91B0)
  (define GL_COMPLETION_STATUS_KHR #x91B1)
  (define GL_CONTEXT_ROBUST_ACCESS #x90F3)
  (define GL_SUBGROUP_SIZE_KHR #x9532)
  (define GL_SUBGROUP_SUPPORTED_STAGES_KHR #x9533)
  (define GL_SUBGROUP_SUPPORTED_FEATURES_KHR #x9534)
  (define GL_SUBGROUP_QUAD_ALL_STAGES_KHR #x9535)
  (define GL_SUBGROUP_FEATURE_BASIC_BIT_KHR #x00000001)
  (define GL_SUBGROUP_FEATURE_VOTE_BIT_KHR #x00000002)
  (define GL_SUBGROUP_FEATURE_ARITHMETIC_BIT_KHR #x00000004)
  (define GL_SUBGROUP_FEATURE_BALLOT_BIT_KHR #x00000008)
  (define GL_SUBGROUP_FEATURE_SHUFFLE_BIT_KHR #x00000010)
  (define GL_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT_KHR #x00000020)
  (define GL_SUBGROUP_FEATURE_CLUSTERED_BIT_KHR #x00000040)
  (define GL_SUBGROUP_FEATURE_QUAD_BIT_KHR #x00000080)
  (define GL_RENDERBUFFER_STORAGE_SAMPLES_AMD #x91B2)
  (define GL_MAX_COLOR_FRAMEBUFFER_SAMPLES_AMD #x91B3)
  (define GL_MAX_COLOR_FRAMEBUFFER_STORAGE_SAMPLES_AMD #x91B4)
  (define GL_MAX_DEPTH_STENCIL_FRAMEBUFFER_SAMPLES_AMD #x91B5)
  (define GL_NUM_SUPPORTED_MULTISAMPLE_MODES_AMD #x91B6)
  (define GL_SUPPORTED_MULTISAMPLE_MODES_AMD #x91B7)
  (define GL_COUNTER_TYPE_AMD #x8BC0)
  (define GL_COUNTER_RANGE_AMD #x8BC1)
  (define GL_UNSIGNED_INT64_AMD #x8BC2)
  (define GL_PERCENTAGE_AMD #x8BC3)
  (define GL_PERFMON_RESULT_AVAILABLE_AMD #x8BC4)
  (define GL_PERFMON_RESULT_SIZE_AMD #x8BC5)
  (define GL_PERFMON_RESULT_AMD #x8BC6)
  (define GL_RGB_422_APPLE #x8A1F)
  (define GL_UNSIGNED_SHORT_8_8_APPLE #x85BA)
  (define GL_UNSIGNED_SHORT_8_8_REV_APPLE #x85BB)
  (define GL_RGB_RAW_422_APPLE #x8A51)
  (define GL_PROGRAM_PIPELINE_OBJECT_EXT #x8A4F)
  (define GL_PROGRAM_OBJECT_EXT #x8B40)
  (define GL_SHADER_OBJECT_EXT #x8B48)
  (define GL_BUFFER_OBJECT_EXT #x9151)
  (define GL_QUERY_OBJECT_EXT #x9153)
  (define GL_VERTEX_ARRAY_OBJECT_EXT #x9154)
  (define GL_PROGRAM_MATRIX_EXT #x8E2D)
  (define GL_TRANSPOSE_PROGRAM_MATRIX_EXT #x8E2E)
  (define GL_PROGRAM_MATRIX_STACK_DEPTH_EXT #x8E2F)
  (define GL_POLYGON_OFFSET_CLAMP_EXT #x8E1B)
  (define GL_RASTER_MULTISAMPLE_EXT #x9327)
  (define GL_RASTER_SAMPLES_EXT #x9328)
  (define GL_MAX_RASTER_SAMPLES_EXT #x9329)
  (define GL_RASTER_FIXED_SAMPLE_LOCATIONS_EXT #x932A)
  (define GL_MULTISAMPLE_RASTERIZATION_ALLOWED_EXT #x932B)
  (define GL_EFFECTIVE_RASTER_SAMPLES_EXT #x932C)
  (define GL_ACTIVE_PROGRAM_EXT #x8B8D)
  (define GL_FRAGMENT_SHADER_DISCARDS_SAMPLES_EXT #x8A52)
  (define GL_COMPRESSED_RGB_S3TC_DXT1_EXT #x83F0)
  (define GL_COMPRESSED_RGBA_S3TC_DXT1_EXT #x83F1)
  (define GL_COMPRESSED_RGBA_S3TC_DXT3_EXT #x83F2)
  (define GL_COMPRESSED_RGBA_S3TC_DXT5_EXT #x83F3)
  (define GL_TEXTURE_REDUCTION_MODE_EXT #x9366)
  (define GL_WEIGHTED_AVERAGE_EXT #x9367)
  (define GL_SR8_EXT #x8FBD)
  (define GL_TEXTURE_SRGB_DECODE_EXT #x8A48)
  (define GL_DECODE_EXT #x8A49)
  (define GL_SKIP_DECODE_EXT #x8A4A)
  (define GL_INCLUSIVE_EXT #x8F10)
  (define GL_EXCLUSIVE_EXT #x8F11)
  (define GL_WINDOW_RECTANGLE_EXT #x8F12)
  (define GL_WINDOW_RECTANGLE_MODE_EXT #x8F13)
  (define GL_MAX_WINDOW_RECTANGLES_EXT #x8F14)
  (define GL_NUM_WINDOW_RECTANGLES_EXT #x8F15)
  (define GL_BLACKHOLE_RENDER_INTEL #x83FC)
  (define GL_CONSERVATIVE_RASTERIZATION_INTEL #x83FE)
  (define GL_PERFQUERY_SINGLE_CONTEXT_INTEL #x00000000)
  (define GL_PERFQUERY_GLOBAL_CONTEXT_INTEL #x00000001)
  (define GL_PERFQUERY_WAIT_INTEL #x83FB)
  (define GL_PERFQUERY_FLUSH_INTEL #x83FA)
  (define GL_PERFQUERY_DONOT_FLUSH_INTEL #x83F9)
  (define GL_PERFQUERY_COUNTER_EVENT_INTEL #x94F0)
  (define GL_PERFQUERY_COUNTER_DURATION_NORM_INTEL #x94F1)
  (define GL_PERFQUERY_COUNTER_DURATION_RAW_INTEL #x94F2)
  (define GL_PERFQUERY_COUNTER_THROUGHPUT_INTEL #x94F3)
  (define GL_PERFQUERY_COUNTER_RAW_INTEL #x94F4)
  (define GL_PERFQUERY_COUNTER_TIMESTAMP_INTEL #x94F5)
  (define GL_PERFQUERY_COUNTER_DATA_UINT32_INTEL #x94F8)
  (define GL_PERFQUERY_COUNTER_DATA_UINT64_INTEL #x94F9)
  (define GL_PERFQUERY_COUNTER_DATA_FLOAT_INTEL #x94FA)
  (define GL_PERFQUERY_COUNTER_DATA_DOUBLE_INTEL #x94FB)
  (define GL_PERFQUERY_COUNTER_DATA_BOOL32_INTEL #x94FC)
  (define GL_PERFQUERY_QUERY_NAME_LENGTH_MAX_INTEL #x94FD)
  (define GL_PERFQUERY_COUNTER_NAME_LENGTH_MAX_INTEL #x94FE)
  (define GL_PERFQUERY_COUNTER_DESC_LENGTH_MAX_INTEL #x94FF)
  (define GL_PERFQUERY_GPA_EXTENDED_COUNTERS_INTEL #x9500)
  (define GL_FRAMEBUFFER_FLIP_Y_MESA #x8BBB)
  (define GL_BLEND_OVERLAP_NV #x9281)
  (define GL_BLEND_PREMULTIPLIED_SRC_NV #x9280)
  (define GL_BLUE_NV #x1905)
  (define GL_COLORBURN_NV #x929A)
  (define GL_COLORDODGE_NV #x9299)
  (define GL_CONJOINT_NV #x9284)
  (define GL_CONTRAST_NV #x92A1)
  (define GL_DARKEN_NV #x9297)
  (define GL_DIFFERENCE_NV #x929E)
  (define GL_DISJOINT_NV #x9283)
  (define GL_DST_ATOP_NV #x928F)
  (define GL_DST_IN_NV #x928B)
  (define GL_DST_NV #x9287)
  (define GL_DST_OUT_NV #x928D)
  (define GL_DST_OVER_NV #x9289)
  (define GL_EXCLUSION_NV #x92A0)
  (define GL_GREEN_NV #x1904)
  (define GL_HARDLIGHT_NV #x929B)
  (define GL_HARDMIX_NV #x92A9)
  (define GL_HSL_COLOR_NV #x92AF)
  (define GL_HSL_HUE_NV #x92AD)
  (define GL_HSL_LUMINOSITY_NV #x92B0)
  (define GL_HSL_SATURATION_NV #x92AE)
  (define GL_INVERT_OVG_NV #x92B4)
  (define GL_INVERT_RGB_NV #x92A3)
  (define GL_LIGHTEN_NV #x9298)
  (define GL_LINEARBURN_NV #x92A5)
  (define GL_LINEARDODGE_NV #x92A4)
  (define GL_LINEARLIGHT_NV #x92A7)
  (define GL_MINUS_CLAMPED_NV #x92B3)
  (define GL_MINUS_NV #x929F)
  (define GL_MULTIPLY_NV #x9294)
  (define GL_OVERLAY_NV #x9296)
  (define GL_PINLIGHT_NV #x92A8)
  (define GL_PLUS_CLAMPED_ALPHA_NV #x92B2)
  (define GL_PLUS_CLAMPED_NV #x92B1)
  (define GL_PLUS_DARKER_NV #x9292)
  (define GL_PLUS_NV #x9291)
  (define GL_RED_NV #x1903)
  (define GL_SCREEN_NV #x9295)
  (define GL_SOFTLIGHT_NV #x929C)
  (define GL_SRC_ATOP_NV #x928E)
  (define GL_SRC_IN_NV #x928A)
  (define GL_SRC_NV #x9286)
  (define GL_SRC_OUT_NV #x928C)
  (define GL_SRC_OVER_NV #x9288)
  (define GL_UNCORRELATED_NV #x9282)
  (define GL_VIVIDLIGHT_NV #x92A6)
  (define GL_XOR_NV #x1506)
  (define GL_BLEND_ADVANCED_COHERENT_NV #x9285)
  (define GL_FACTOR_MIN_AMD #x901C)
  (define GL_FACTOR_MAX_AMD #x901D)
  (define GL_VIEWPORT_POSITION_W_SCALE_NV #x937C)
  (define GL_VIEWPORT_POSITION_W_SCALE_X_COEFF_NV #x937D)
  (define GL_VIEWPORT_POSITION_W_SCALE_Y_COEFF_NV #x937E)
  (define GL_TERMINATE_SEQUENCE_COMMAND_NV #x0000)
  (define GL_NOP_COMMAND_NV #x0001)
  (define GL_DRAW_ELEMENTS_COMMAND_NV #x0002)
  (define GL_DRAW_ARRAYS_COMMAND_NV #x0003)
  (define GL_DRAW_ELEMENTS_STRIP_COMMAND_NV #x0004)
  (define GL_DRAW_ARRAYS_STRIP_COMMAND_NV #x0005)
  (define GL_DRAW_ELEMENTS_INSTANCED_COMMAND_NV #x0006)
  (define GL_DRAW_ARRAYS_INSTANCED_COMMAND_NV #x0007)
  (define GL_ELEMENT_ADDRESS_COMMAND_NV #x0008)
  (define GL_ATTRIBUTE_ADDRESS_COMMAND_NV #x0009)
  (define GL_UNIFORM_ADDRESS_COMMAND_NV #x000A)
  (define GL_BLEND_COLOR_COMMAND_NV #x000B)
  (define GL_STENCIL_REF_COMMAND_NV #x000C)
  (define GL_LINE_WIDTH_COMMAND_NV #x000D)
  (define GL_POLYGON_OFFSET_COMMAND_NV #x000E)
  (define GL_ALPHA_REF_COMMAND_NV #x000F)
  (define GL_VIEWPORT_COMMAND_NV #x0010)
  (define GL_SCISSOR_COMMAND_NV #x0011)
  (define GL_FRONT_FACE_COMMAND_NV #x0012)
  (define GL_QUERY_WAIT_NV #x8E13)
  (define GL_QUERY_NO_WAIT_NV #x8E14)
  (define GL_QUERY_BY_REGION_WAIT_NV #x8E15)
  (define GL_QUERY_BY_REGION_NO_WAIT_NV #x8E16)
  (define GL_CONSERVATIVE_RASTERIZATION_NV #x9346)
  (define GL_SUBPIXEL_PRECISION_BIAS_X_BITS_NV #x9347)
  (define GL_SUBPIXEL_PRECISION_BIAS_Y_BITS_NV #x9348)
  (define GL_MAX_SUBPIXEL_PRECISION_BIAS_BITS_NV #x9349)
  (define GL_CONSERVATIVE_RASTER_DILATE_NV #x9379)
  (define GL_CONSERVATIVE_RASTER_DILATE_RANGE_NV #x937A)
  (define GL_CONSERVATIVE_RASTER_DILATE_GRANULARITY_NV #x937B)
  (define GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_NV #x9550)
  (define GL_CONSERVATIVE_RASTER_MODE_NV #x954D)
  (define GL_CONSERVATIVE_RASTER_MODE_POST_SNAP_NV #x954E)
  (define GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_TRIANGLES_NV #x954F)
  (define GL_DEPTH_COMPONENT32F_NV #x8DAB)
  (define GL_DEPTH32F_STENCIL8_NV #x8DAC)
  (define GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV #x8DAD)
  (define GL_DEPTH_BUFFER_FLOAT_MODE_NV #x8DAF)
  (define GL_FILL_RECTANGLE_NV #x933C)
  (define GL_FRAGMENT_COVERAGE_TO_COLOR_NV #x92DD)
  (define GL_FRAGMENT_COVERAGE_COLOR_NV #x92DE)
  (define GL_COVERAGE_MODULATION_TABLE_NV #x9331)
  (define GL_COLOR_SAMPLES_NV #x8E20)
  (define GL_DEPTH_SAMPLES_NV #x932D)
  (define GL_STENCIL_SAMPLES_NV #x932E)
  (define GL_MIXED_DEPTH_SAMPLES_SUPPORTED_NV #x932F)
  (define GL_MIXED_STENCIL_SAMPLES_SUPPORTED_NV #x9330)
  (define GL_COVERAGE_MODULATION_NV #x9332)
  (define GL_COVERAGE_MODULATION_TABLE_SIZE_NV #x9333)
  (define GL_RENDERBUFFER_COVERAGE_SAMPLES_NV #x8CAB)
  (define GL_RENDERBUFFER_COLOR_SAMPLES_NV #x8E10)
  (define GL_MAX_MULTISAMPLE_COVERAGE_MODES_NV #x8E11)
  (define GL_MULTISAMPLE_COVERAGE_MODES_NV #x8E12)
  (define GL_INT64_NV #x140E)
  (define GL_UNSIGNED_INT64_NV #x140F)
  (define GL_INT8_NV #x8FE0)
  (define GL_INT8_VEC2_NV #x8FE1)
  (define GL_INT8_VEC3_NV #x8FE2)
  (define GL_INT8_VEC4_NV #x8FE3)
  (define GL_INT16_NV #x8FE4)
  (define GL_INT16_VEC2_NV #x8FE5)
  (define GL_INT16_VEC3_NV #x8FE6)
  (define GL_INT16_VEC4_NV #x8FE7)
  (define GL_INT64_VEC2_NV #x8FE9)
  (define GL_INT64_VEC3_NV #x8FEA)
  (define GL_INT64_VEC4_NV #x8FEB)
  (define GL_UNSIGNED_INT8_NV #x8FEC)
  (define GL_UNSIGNED_INT8_VEC2_NV #x8FED)
  (define GL_UNSIGNED_INT8_VEC3_NV #x8FEE)
  (define GL_UNSIGNED_INT8_VEC4_NV #x8FEF)
  (define GL_UNSIGNED_INT16_NV #x8FF0)
  (define GL_UNSIGNED_INT16_VEC2_NV #x8FF1)
  (define GL_UNSIGNED_INT16_VEC3_NV #x8FF2)
  (define GL_UNSIGNED_INT16_VEC4_NV #x8FF3)
  (define GL_UNSIGNED_INT64_VEC2_NV #x8FF5)
  (define GL_UNSIGNED_INT64_VEC3_NV #x8FF6)
  (define GL_UNSIGNED_INT64_VEC4_NV #x8FF7)
  (define GL_FLOAT16_NV #x8FF8)
  (define GL_FLOAT16_VEC2_NV #x8FF9)
  (define GL_FLOAT16_VEC3_NV #x8FFA)
  (define GL_FLOAT16_VEC4_NV #x8FFB)
  (define GL_MULTISAMPLES_NV #x9371)
  (define GL_SUPERSAMPLE_SCALE_X_NV #x9372)
  (define GL_SUPERSAMPLE_SCALE_Y_NV #x9373)
  (define GL_CONFORMANT_NV #x9374)
  (define GL_ATTACHED_MEMORY_OBJECT_NV #x95A4)
  (define GL_ATTACHED_MEMORY_OFFSET_NV #x95A5)
  (define GL_MEMORY_ATTACHABLE_ALIGNMENT_NV #x95A6)
  (define GL_MEMORY_ATTACHABLE_SIZE_NV #x95A7)
  (define GL_MEMORY_ATTACHABLE_NV #x95A8)
  (define GL_DETACHED_MEMORY_INCARNATION_NV #x95A9)
  (define GL_DETACHED_TEXTURES_NV #x95AA)
  (define GL_DETACHED_BUFFERS_NV #x95AB)
  (define GL_MAX_DETACHED_TEXTURES_NV #x95AC)
  (define GL_MAX_DETACHED_BUFFERS_NV #x95AD)
  (define GL_MESH_SHADER_NV #x9559)
  (define GL_TASK_SHADER_NV #x955A)
  (define GL_MAX_MESH_UNIFORM_BLOCKS_NV #x8E60)
  (define GL_MAX_MESH_TEXTURE_IMAGE_UNITS_NV #x8E61)
  (define GL_MAX_MESH_IMAGE_UNIFORMS_NV #x8E62)
  (define GL_MAX_MESH_UNIFORM_COMPONENTS_NV #x8E63)
  (define GL_MAX_MESH_ATOMIC_COUNTER_BUFFERS_NV #x8E64)
  (define GL_MAX_MESH_ATOMIC_COUNTERS_NV #x8E65)
  (define GL_MAX_MESH_SHADER_STORAGE_BLOCKS_NV #x8E66)
  (define GL_MAX_COMBINED_MESH_UNIFORM_COMPONENTS_NV #x8E67)
  (define GL_MAX_TASK_UNIFORM_BLOCKS_NV #x8E68)
  (define GL_MAX_TASK_TEXTURE_IMAGE_UNITS_NV #x8E69)
  (define GL_MAX_TASK_IMAGE_UNIFORMS_NV #x8E6A)
  (define GL_MAX_TASK_UNIFORM_COMPONENTS_NV #x8E6B)
  (define GL_MAX_TASK_ATOMIC_COUNTER_BUFFERS_NV #x8E6C)
  (define GL_MAX_TASK_ATOMIC_COUNTERS_NV #x8E6D)
  (define GL_MAX_TASK_SHADER_STORAGE_BLOCKS_NV #x8E6E)
  (define GL_MAX_COMBINED_TASK_UNIFORM_COMPONENTS_NV #x8E6F)
  (define GL_MAX_MESH_WORK_GROUP_INVOCATIONS_NV #x95A2)
  (define GL_MAX_TASK_WORK_GROUP_INVOCATIONS_NV #x95A3)
  (define GL_MAX_MESH_TOTAL_MEMORY_SIZE_NV #x9536)
  (define GL_MAX_TASK_TOTAL_MEMORY_SIZE_NV #x9537)
  (define GL_MAX_MESH_OUTPUT_VERTICES_NV #x9538)
  (define GL_MAX_MESH_OUTPUT_PRIMITIVES_NV #x9539)
  (define GL_MAX_TASK_OUTPUT_COUNT_NV #x953A)
  (define GL_MAX_DRAW_MESH_TASKS_COUNT_NV #x953D)
  (define GL_MAX_MESH_VIEWS_NV #x9557)
  (define GL_MESH_OUTPUT_PER_VERTEX_GRANULARITY_NV #x92DF)
  (define GL_MESH_OUTPUT_PER_PRIMITIVE_GRANULARITY_NV #x9543)
  (define GL_MAX_MESH_WORK_GROUP_SIZE_NV #x953B)
  (define GL_MAX_TASK_WORK_GROUP_SIZE_NV #x953C)
  (define GL_MESH_WORK_GROUP_SIZE_NV #x953E)
  (define GL_TASK_WORK_GROUP_SIZE_NV #x953F)
  (define GL_MESH_VERTICES_OUT_NV #x9579)
  (define GL_MESH_PRIMITIVES_OUT_NV #x957A)
  (define GL_MESH_OUTPUT_TYPE_NV #x957B)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_MESH_SHADER_NV #x959C)
  (define GL_UNIFORM_BLOCK_REFERENCED_BY_TASK_SHADER_NV #x959D)
  (define GL_REFERENCED_BY_MESH_SHADER_NV #x95A0)
  (define GL_REFERENCED_BY_TASK_SHADER_NV #x95A1)
  (define GL_MESH_SHADER_BIT_NV #x00000040)
  (define GL_TASK_SHADER_BIT_NV #x00000080)
  (define GL_MESH_SUBROUTINE_NV #x957C)
  (define GL_TASK_SUBROUTINE_NV #x957D)
  (define GL_MESH_SUBROUTINE_UNIFORM_NV #x957E)
  (define GL_TASK_SUBROUTINE_UNIFORM_NV #x957F)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_MESH_SHADER_NV #x959E)
  (define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TASK_SHADER_NV #x959F)
  (define GL_PATH_FORMAT_SVG_NV #x9070)
  (define GL_PATH_FORMAT_PS_NV #x9071)
  (define GL_STANDARD_FONT_NAME_NV #x9072)
  (define GL_SYSTEM_FONT_NAME_NV #x9073)
  (define GL_FILE_NAME_NV #x9074)
  (define GL_PATH_STROKE_WIDTH_NV #x9075)
  (define GL_PATH_END_CAPS_NV #x9076)
  (define GL_PATH_INITIAL_END_CAP_NV #x9077)
  (define GL_PATH_TERMINAL_END_CAP_NV #x9078)
  (define GL_PATH_JOIN_STYLE_NV #x9079)
  (define GL_PATH_MITER_LIMIT_NV #x907A)
  (define GL_PATH_DASH_CAPS_NV #x907B)
  (define GL_PATH_INITIAL_DASH_CAP_NV #x907C)
  (define GL_PATH_TERMINAL_DASH_CAP_NV #x907D)
  (define GL_PATH_DASH_OFFSET_NV #x907E)
  (define GL_PATH_CLIENT_LENGTH_NV #x907F)
  (define GL_PATH_FILL_MODE_NV #x9080)
  (define GL_PATH_FILL_MASK_NV #x9081)
  (define GL_PATH_FILL_COVER_MODE_NV #x9082)
  (define GL_PATH_STROKE_COVER_MODE_NV #x9083)
  (define GL_PATH_STROKE_MASK_NV #x9084)
  (define GL_COUNT_UP_NV #x9088)
  (define GL_COUNT_DOWN_NV #x9089)
  (define GL_PATH_OBJECT_BOUNDING_BOX_NV #x908A)
  (define GL_CONVEX_HULL_NV #x908B)
  (define GL_BOUNDING_BOX_NV #x908D)
  (define GL_TRANSLATE_X_NV #x908E)
  (define GL_TRANSLATE_Y_NV #x908F)
  (define GL_TRANSLATE_2D_NV #x9090)
  (define GL_TRANSLATE_3D_NV #x9091)
  (define GL_AFFINE_2D_NV #x9092)
  (define GL_AFFINE_3D_NV #x9094)
  (define GL_TRANSPOSE_AFFINE_2D_NV #x9096)
  (define GL_TRANSPOSE_AFFINE_3D_NV #x9098)
  (define GL_UTF8_NV #x909A)
  (define GL_UTF16_NV #x909B)
  (define GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV #x909C)
  (define GL_PATH_COMMAND_COUNT_NV #x909D)
  (define GL_PATH_COORD_COUNT_NV #x909E)
  (define GL_PATH_DASH_ARRAY_COUNT_NV #x909F)
  (define GL_PATH_COMPUTED_LENGTH_NV #x90A0)
  (define GL_PATH_FILL_BOUNDING_BOX_NV #x90A1)
  (define GL_PATH_STROKE_BOUNDING_BOX_NV #x90A2)
  (define GL_SQUARE_NV #x90A3)
  (define GL_ROUND_NV #x90A4)
  (define GL_TRIANGULAR_NV #x90A5)
  (define GL_BEVEL_NV #x90A6)
  (define GL_MITER_REVERT_NV #x90A7)
  (define GL_MITER_TRUNCATE_NV #x90A8)
  (define GL_SKIP_MISSING_GLYPH_NV #x90A9)
  (define GL_USE_MISSING_GLYPH_NV #x90AA)
  (define GL_PATH_ERROR_POSITION_NV #x90AB)
  (define GL_ACCUM_ADJACENT_PAIRS_NV #x90AD)
  (define GL_ADJACENT_PAIRS_NV #x90AE)
  (define GL_FIRST_TO_REST_NV #x90AF)
  (define GL_PATH_GEN_MODE_NV #x90B0)
  (define GL_PATH_GEN_COEFF_NV #x90B1)
  (define GL_PATH_GEN_COMPONENTS_NV #x90B3)
  (define GL_PATH_STENCIL_FUNC_NV #x90B7)
  (define GL_PATH_STENCIL_REF_NV #x90B8)
  (define GL_PATH_STENCIL_VALUE_MASK_NV #x90B9)
  (define GL_PATH_STENCIL_DEPTH_OFFSET_FACTOR_NV #x90BD)
  (define GL_PATH_STENCIL_DEPTH_OFFSET_UNITS_NV #x90BE)
  (define GL_PATH_COVER_DEPTH_FUNC_NV #x90BF)
  (define GL_PATH_DASH_OFFSET_RESET_NV #x90B4)
  (define GL_MOVE_TO_RESETS_NV #x90B5)
  (define GL_MOVE_TO_CONTINUES_NV #x90B6)
  (define GL_CLOSE_PATH_NV #x00)
  (define GL_MOVE_TO_NV #x02)
  (define GL_RELATIVE_MOVE_TO_NV #x03)
  (define GL_LINE_TO_NV #x04)
  (define GL_RELATIVE_LINE_TO_NV #x05)
  (define GL_HORIZONTAL_LINE_TO_NV #x06)
  (define GL_RELATIVE_HORIZONTAL_LINE_TO_NV #x07)
  (define GL_VERTICAL_LINE_TO_NV #x08)
  (define GL_RELATIVE_VERTICAL_LINE_TO_NV #x09)
  (define GL_QUADRATIC_CURVE_TO_NV #x0A)
  (define GL_RELATIVE_QUADRATIC_CURVE_TO_NV #x0B)
  (define GL_CUBIC_CURVE_TO_NV #x0C)
  (define GL_RELATIVE_CUBIC_CURVE_TO_NV #x0D)
  (define GL_SMOOTH_QUADRATIC_CURVE_TO_NV #x0E)
  (define GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV #x0F)
  (define GL_SMOOTH_CUBIC_CURVE_TO_NV #x10)
  (define GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV #x11)
  (define GL_SMALL_CCW_ARC_TO_NV #x12)
  (define GL_RELATIVE_SMALL_CCW_ARC_TO_NV #x13)
  (define GL_SMALL_CW_ARC_TO_NV #x14)
  (define GL_RELATIVE_SMALL_CW_ARC_TO_NV #x15)
  (define GL_LARGE_CCW_ARC_TO_NV #x16)
  (define GL_RELATIVE_LARGE_CCW_ARC_TO_NV #x17)
  (define GL_LARGE_CW_ARC_TO_NV #x18)
  (define GL_RELATIVE_LARGE_CW_ARC_TO_NV #x19)
  (define GL_RESTART_PATH_NV #xF0)
  (define GL_DUP_FIRST_CUBIC_CURVE_TO_NV #xF2)
  (define GL_DUP_LAST_CUBIC_CURVE_TO_NV #xF4)
  (define GL_RECT_NV #xF6)
  (define GL_CIRCULAR_CCW_ARC_TO_NV #xF8)
  (define GL_CIRCULAR_CW_ARC_TO_NV #xFA)
  (define GL_CIRCULAR_TANGENT_ARC_TO_NV #xFC)
  (define GL_ARC_TO_NV #xFE)
  (define GL_RELATIVE_ARC_TO_NV #xFF)
  (define GL_BOLD_BIT_NV #x01)
  (define GL_ITALIC_BIT_NV #x02)
  (define GL_GLYPH_WIDTH_BIT_NV #x01)
  (define GL_GLYPH_HEIGHT_BIT_NV #x02)
  (define GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV #x04)
  (define GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV #x08)
  (define GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV #x10)
  (define GL_GLYPH_VERTICAL_BEARING_X_BIT_NV #x20)
  (define GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV #x40)
  (define GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV #x80)
  (define GL_GLYPH_HAS_KERNING_BIT_NV #x100)
  (define GL_FONT_X_MIN_BOUNDS_BIT_NV #x00010000)
  (define GL_FONT_Y_MIN_BOUNDS_BIT_NV #x00020000)
  (define GL_FONT_X_MAX_BOUNDS_BIT_NV #x00040000)
  (define GL_FONT_Y_MAX_BOUNDS_BIT_NV #x00080000)
  (define GL_FONT_UNITS_PER_EM_BIT_NV #x00100000)
  (define GL_FONT_ASCENDER_BIT_NV #x00200000)
  (define GL_FONT_DESCENDER_BIT_NV #x00400000)
  (define GL_FONT_HEIGHT_BIT_NV #x00800000)
  (define GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV #x01000000)
  (define GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV #x02000000)
  (define GL_FONT_UNDERLINE_POSITION_BIT_NV #x04000000)
  (define GL_FONT_UNDERLINE_THICKNESS_BIT_NV #x08000000)
  (define GL_FONT_HAS_KERNING_BIT_NV #x10000000)
  (define GL_ROUNDED_RECT_NV #xE8)
  (define GL_RELATIVE_ROUNDED_RECT_NV #xE9)
  (define GL_ROUNDED_RECT2_NV #xEA)
  (define GL_RELATIVE_ROUNDED_RECT2_NV #xEB)
  (define GL_ROUNDED_RECT4_NV #xEC)
  (define GL_RELATIVE_ROUNDED_RECT4_NV #xED)
  (define GL_ROUNDED_RECT8_NV #xEE)
  (define GL_RELATIVE_ROUNDED_RECT8_NV #xEF)
  (define GL_RELATIVE_RECT_NV #xF7)
  (define GL_FONT_GLYPHS_AVAILABLE_NV #x9368)
  (define GL_FONT_TARGET_UNAVAILABLE_NV #x9369)
  (define GL_FONT_UNAVAILABLE_NV #x936A)
  (define GL_FONT_UNINTELLIGIBLE_NV #x936B)
  (define GL_CONIC_CURVE_TO_NV #x1A)
  (define GL_RELATIVE_CONIC_CURVE_TO_NV #x1B)
  (define GL_FONT_NUM_GLYPH_INDICES_BIT_NV #x20000000)
  (define GL_STANDARD_FONT_FORMAT_NV #x936C)
  (define GL_PATH_PROJECTION_NV #x1701)
  (define GL_PATH_MODELVIEW_NV #x1700)
  (define GL_PATH_MODELVIEW_STACK_DEPTH_NV #x0BA3)
  (define GL_PATH_MODELVIEW_MATRIX_NV #x0BA6)
  (define GL_PATH_MAX_MODELVIEW_STACK_DEPTH_NV #x0D36)
  (define GL_PATH_TRANSPOSE_MODELVIEW_MATRIX_NV #x84E3)
  (define GL_PATH_PROJECTION_STACK_DEPTH_NV #x0BA4)
  (define GL_PATH_PROJECTION_MATRIX_NV #x0BA7)
  (define GL_PATH_MAX_PROJECTION_STACK_DEPTH_NV #x0D38)
  (define GL_PATH_TRANSPOSE_PROJECTION_MATRIX_NV #x84E4)
  (define GL_FRAGMENT_INPUT_NV #x936D)
  (define GL_SHARED_EDGE_NV #xC0)
  (define GL_REPRESENTATIVE_FRAGMENT_TEST_NV #x937F)
  (define GL_SAMPLE_LOCATION_SUBPIXEL_BITS_NV #x933D)
  (define GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_NV #x933E)
  (define GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_NV #x933F)
  (define GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_NV #x9340)
  (define GL_SAMPLE_LOCATION_NV #x8E50)
  (define GL_PROGRAMMABLE_SAMPLE_LOCATION_NV #x9341)
  (define GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_NV #x9342)
  (define GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_NV #x9343)
  (define GL_SCISSOR_TEST_EXCLUSIVE_NV #x9555)
  (define GL_SCISSOR_BOX_EXCLUSIVE_NV #x9556)
  (define GL_BUFFER_GPU_ADDRESS_NV #x8F1D)
  (define GL_GPU_ADDRESS_NV #x8F34)
  (define GL_MAX_SHADER_BUFFER_ADDRESS_NV #x8F35)
  (define GL_SHADER_GLOBAL_ACCESS_BARRIER_BIT_NV #x00000010)
  (define GL_SUBGROUP_FEATURE_PARTITIONED_BIT_NV #x00000100)
  (define GL_WARP_SIZE_NV #x9339)
  (define GL_WARPS_PER_SM_NV #x933A)
  (define GL_SM_COUNT_NV #x933B)
  (define GL_SHADING_RATE_IMAGE_NV #x9563)
  (define GL_SHADING_RATE_NO_INVOCATIONS_NV #x9564)
  (define GL_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV #x9565)
  (define GL_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV #x9566)
  (define GL_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV #x9567)
  (define GL_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV #x9568)
  (define GL_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV #x9569)
  (define GL_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV #x956A)
  (define GL_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV #x956B)
  (define GL_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV #x956C)
  (define GL_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV #x956D)
  (define GL_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV #x956E)
  (define GL_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV #x956F)
  (define GL_SHADING_RATE_IMAGE_BINDING_NV #x955B)
  (define GL_SHADING_RATE_IMAGE_TEXEL_WIDTH_NV #x955C)
  (define GL_SHADING_RATE_IMAGE_TEXEL_HEIGHT_NV #x955D)
  (define GL_SHADING_RATE_IMAGE_PALETTE_SIZE_NV #x955E)
  (define GL_MAX_COARSE_FRAGMENT_SAMPLES_NV #x955F)
  (define GL_SHADING_RATE_SAMPLE_ORDER_DEFAULT_NV #x95AE)
  (define GL_SHADING_RATE_SAMPLE_ORDER_PIXEL_MAJOR_NV #x95AF)
  (define GL_SHADING_RATE_SAMPLE_ORDER_SAMPLE_MAJOR_NV #x95B0)
  (define GL_UNIFORM_BUFFER_UNIFIED_NV #x936E)
  (define GL_UNIFORM_BUFFER_ADDRESS_NV #x936F)
  (define GL_UNIFORM_BUFFER_LENGTH_NV #x9370)
  (define GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV #x8F1E)
  (define GL_ELEMENT_ARRAY_UNIFIED_NV #x8F1F)
  (define GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV #x8F20)
  (define GL_VERTEX_ARRAY_ADDRESS_NV #x8F21)
  (define GL_NORMAL_ARRAY_ADDRESS_NV #x8F22)
  (define GL_COLOR_ARRAY_ADDRESS_NV #x8F23)
  (define GL_INDEX_ARRAY_ADDRESS_NV #x8F24)
  (define GL_TEXTURE_COORD_ARRAY_ADDRESS_NV #x8F25)
  (define GL_EDGE_FLAG_ARRAY_ADDRESS_NV #x8F26)
  (define GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV #x8F27)
  (define GL_FOG_COORD_ARRAY_ADDRESS_NV #x8F28)
  (define GL_ELEMENT_ARRAY_ADDRESS_NV #x8F29)
  (define GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV #x8F2A)
  (define GL_VERTEX_ARRAY_LENGTH_NV #x8F2B)
  (define GL_NORMAL_ARRAY_LENGTH_NV #x8F2C)
  (define GL_COLOR_ARRAY_LENGTH_NV #x8F2D)
  (define GL_INDEX_ARRAY_LENGTH_NV #x8F2E)
  (define GL_TEXTURE_COORD_ARRAY_LENGTH_NV #x8F2F)
  (define GL_EDGE_FLAG_ARRAY_LENGTH_NV #x8F30)
  (define GL_SECONDARY_COLOR_ARRAY_LENGTH_NV #x8F31)
  (define GL_FOG_COORD_ARRAY_LENGTH_NV #x8F32)
  (define GL_ELEMENT_ARRAY_LENGTH_NV #x8F33)
  (define GL_DRAW_INDIRECT_UNIFIED_NV #x8F40)
  (define GL_DRAW_INDIRECT_ADDRESS_NV #x8F41)
  (define GL_DRAW_INDIRECT_LENGTH_NV #x8F42)
  (define GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV #x9350)
  (define GL_VIEWPORT_SWIZZLE_NEGATIVE_X_NV #x9351)
  (define GL_VIEWPORT_SWIZZLE_POSITIVE_Y_NV #x9352)
  (define GL_VIEWPORT_SWIZZLE_NEGATIVE_Y_NV #x9353)
  (define GL_VIEWPORT_SWIZZLE_POSITIVE_Z_NV #x9354)
  (define GL_VIEWPORT_SWIZZLE_NEGATIVE_Z_NV #x9355)
  (define GL_VIEWPORT_SWIZZLE_POSITIVE_W_NV #x9356)
  (define GL_VIEWPORT_SWIZZLE_NEGATIVE_W_NV #x9357)
  (define GL_VIEWPORT_SWIZZLE_X_NV #x9358)
  (define GL_VIEWPORT_SWIZZLE_Y_NV #x9359)
  (define GL_VIEWPORT_SWIZZLE_Z_NV #x935A)
  (define GL_VIEWPORT_SWIZZLE_W_NV #x935B)
  (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_NUM_VIEWS_OVR #x9630)
  (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_BASE_VIEW_INDEX_OVR #x9632)
  (define GL_MAX_VIEWS_OVR #x9631)
  (define GL_FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR #x9633)
  ;; void glCullFace(GLenum mode)
  (define-cdecl void glCullFace (unsigned-int))
  ;; void glFrontFace(GLenum mode)
  (define-cdecl void glFrontFace (unsigned-int))
  ;; void glHint(GLenum target, GLenum mode)
  (define-cdecl void glHint (unsigned-int unsigned-int))
  ;; void glLineWidth(GLfloat width)
  (define-cdecl void glLineWidth (float))
  ;; void glPointSize(GLfloat size)
  (define-cdecl void glPointSize (float))
  ;; void glPolygonMode(GLenum face, GLenum mode)
  (define-cdecl void glPolygonMode (unsigned-int unsigned-int))
  ;; void glScissor(GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glScissor (int int int int))
  ;; void glTexParameterf(GLenum target, GLenum pname, GLfloat param)
  (define-cdecl void glTexParameterf (unsigned-int unsigned-int float))
  ;; void glTexParameterfv(GLenum target, GLenum pname, const GLfloat *params)
  (define-cdecl void glTexParameterfv (unsigned-int unsigned-int void*))
  ;; void glTexParameteri(GLenum target, GLenum pname, GLint param)
  (define-cdecl void glTexParameteri (unsigned-int unsigned-int int))
  ;; void glTexParameteriv(GLenum target, GLenum pname, const GLint *params)
  (define-cdecl void glTexParameteriv (unsigned-int unsigned-int void*))
  ;; void glTexImage1D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTexImage1D (unsigned-int int int int int unsigned-int unsigned-int void*))
  ;; void glTexImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTexImage2D (unsigned-int int int int int int unsigned-int unsigned-int void*))
  ;; void glDrawBuffer(GLenum buf)
  (define-cdecl void glDrawBuffer (unsigned-int))
  ;; void glClear(GLbitfield mask)
  (define-cdecl void glClear (unsigned-int))
  ;; void glClearColor(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
  (define-cdecl void glClearColor (float float float float))
  ;; void glClearStencil(GLint s)
  (define-cdecl void glClearStencil (int))
  ;; void glClearDepth(GLdouble depth)
  (define-cdecl void glClearDepth (double))
  ;; void glStencilMask(GLuint mask)
  (define-cdecl void glStencilMask (unsigned-int))
  ;; void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)
  (define-cdecl void glColorMask (uint8_t uint8_t uint8_t uint8_t))
  ;; void glDepthMask(GLboolean flag)
  (define-cdecl void glDepthMask (uint8_t))
  ;; void glDisable(GLenum cap)
  (define-cdecl void glDisable (unsigned-int))
  ;; void glEnable(GLenum cap)
  (define-cdecl void glEnable (unsigned-int))
  ;; void glFinish(void)
  (define-cdecl void glFinish ())
  ;; void glFlush(void)
  (define-cdecl void glFlush ())
  ;; void glBlendFunc(GLenum sfactor, GLenum dfactor)
  (define-cdecl void glBlendFunc (unsigned-int unsigned-int))
  ;; void glLogicOp(GLenum opcode)
  (define-cdecl void glLogicOp (unsigned-int))
  ;; void glStencilFunc(GLenum func, GLint ref, GLuint mask)
  (define-cdecl void glStencilFunc (unsigned-int int unsigned-int))
  ;; void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass)
  (define-cdecl void glStencilOp (unsigned-int unsigned-int unsigned-int))
  ;; void glDepthFunc(GLenum func)
  (define-cdecl void glDepthFunc (unsigned-int))
  ;; void glPixelStoref(GLenum pname, GLfloat param)
  (define-cdecl void glPixelStoref (unsigned-int float))
  ;; void glPixelStorei(GLenum pname, GLint param)
  (define-cdecl void glPixelStorei (unsigned-int int))
  ;; void glReadBuffer(GLenum src)
  (define-cdecl void glReadBuffer (unsigned-int))
  ;; void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels)
  (define-cdecl void glReadPixels (int int int int unsigned-int unsigned-int void*))
  ;; void glGetBooleanv(GLenum pname, GLboolean *data)
  (define-cdecl void glGetBooleanv (unsigned-int void*))
  ;; void glGetDoublev(GLenum pname, GLdouble *data)
  (define-cdecl void glGetDoublev (unsigned-int void*))
  ;; GLenum glGetError(void)
  (define-cdecl unsigned-int glGetError ())
  ;; void glGetFloatv(GLenum pname, GLfloat *data)
  (define-cdecl void glGetFloatv (unsigned-int void*))
  ;; void glGetIntegerv(GLenum pname, GLint *data)
  (define-cdecl void glGetIntegerv (unsigned-int void*))
  ;; void glGetTexImage(GLenum target, GLint level, GLenum format, GLenum type, void *pixels)
  (define-cdecl void glGetTexImage (unsigned-int int unsigned-int unsigned-int void*))
  ;; void glGetTexParameterfv(GLenum target, GLenum pname, GLfloat *params)
  (define-cdecl void glGetTexParameterfv (unsigned-int unsigned-int void*))
  ;; void glGetTexParameteriv(GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetTexParameteriv (unsigned-int unsigned-int void*))
  ;; void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, GLfloat *params)
  (define-cdecl void glGetTexLevelParameterfv (unsigned-int int unsigned-int void*))
  ;; void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, GLint *params)
  (define-cdecl void glGetTexLevelParameteriv (unsigned-int int unsigned-int void*))
  ;; GLboolean glIsEnabled(GLenum cap)
  (define-cdecl uint8_t glIsEnabled (unsigned-int))
  ;; void glDepthRange(GLdouble n, GLdouble f)
  (define-cdecl void glDepthRange (double double))
  ;; void glViewport(GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glViewport (int int int int))
  ;; void glDrawArrays(GLenum mode, GLint first, GLsizei count)
  (define-cdecl void glDrawArrays (unsigned-int int int))
  ;; void glDrawElements(GLenum mode, GLsizei count, GLenum type, const void *indices)
  (define-cdecl void glDrawElements (unsigned-int int unsigned-int void*))
  ;; void glGetPointerv(GLenum pname, void **params)
  (define-cdecl void glGetPointerv (unsigned-int void*))
  ;; void glPolygonOffset(GLfloat factor, GLfloat units)
  (define-cdecl void glPolygonOffset (float float))
  ;; void glCopyTexImage1D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)
  (define-cdecl void glCopyTexImage1D (unsigned-int int unsigned-int int int int int))
  ;; void glCopyTexImage2D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)
  (define-cdecl void glCopyTexImage2D (unsigned-int int unsigned-int int int int int int))
  ;; void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)
  (define-cdecl void glCopyTexSubImage1D (unsigned-int int int int int int))
  ;; void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyTexSubImage2D (unsigned-int int int int int int int int))
  ;; void glTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTexSubImage1D (unsigned-int int int int unsigned-int unsigned-int void*))
  ;; void glTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTexSubImage2D (unsigned-int int int int int int unsigned-int unsigned-int void*))
  ;; void glBindTexture(GLenum target, GLuint texture)
  (define-cdecl void glBindTexture (unsigned-int unsigned-int))
  ;; void glDeleteTextures(GLsizei n, const GLuint *textures)
  (define-cdecl void glDeleteTextures (int void*))
  ;; void glGenTextures(GLsizei n, GLuint *textures)
  (define-cdecl void glGenTextures (int void*))
  ;; GLboolean glIsTexture(GLuint texture)
  (define-cdecl uint8_t glIsTexture (unsigned-int))
  ;; void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices)
  (define-cdecl void glDrawRangeElements (unsigned-int unsigned-int unsigned-int int unsigned-int void*))
  ;; void glTexImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTexImage3D (unsigned-int int int int int int int unsigned-int unsigned-int void*))
  ;; void glTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTexSubImage3D (unsigned-int int int int int int int int unsigned-int unsigned-int void*))
  ;; void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyTexSubImage3D (unsigned-int int int int int int int int int))
  ;; void glActiveTexture(GLenum texture)
  (define-cdecl void glActiveTexture (unsigned-int))
  ;; void glSampleCoverage(GLfloat value, GLboolean invert)
  (define-cdecl void glSampleCoverage (float uint8_t))
  ;; void glCompressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTexImage3D (unsigned-int int unsigned-int int int int int int void*))
  ;; void glCompressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTexImage2D (unsigned-int int unsigned-int int int int int void*))
  ;; void glCompressedTexImage1D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTexImage1D (unsigned-int int unsigned-int int int int void*))
  ;; void glCompressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTexSubImage3D (unsigned-int int int int int int int int unsigned-int int void*))
  ;; void glCompressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTexSubImage2D (unsigned-int int int int int int unsigned-int int void*))
  ;; void glCompressedTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTexSubImage1D (unsigned-int int int int unsigned-int int void*))
  ;; void glGetCompressedTexImage(GLenum target, GLint level, void *img)
  (define-cdecl void glGetCompressedTexImage (unsigned-int int void*))
  ;; void glBlendFuncSeparate(GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha)
  (define-cdecl void glBlendFuncSeparate (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glMultiDrawArrays(GLenum mode, const GLint *first, const GLsizei *count, GLsizei drawcount)
  (define-cdecl void glMultiDrawArrays (unsigned-int void* void* int))
  ;; void glMultiDrawElements(GLenum mode, const GLsizei *count, GLenum type, const void *const*indices, GLsizei drawcount)
  (define-cdecl void glMultiDrawElements (unsigned-int void* unsigned-int void* int))
  ;; void glPointParameterf(GLenum pname, GLfloat param)
  (define-cdecl void glPointParameterf (unsigned-int float))
  ;; void glPointParameterfv(GLenum pname, const GLfloat *params)
  (define-cdecl void glPointParameterfv (unsigned-int void*))
  ;; void glPointParameteri(GLenum pname, GLint param)
  (define-cdecl void glPointParameteri (unsigned-int int))
  ;; void glPointParameteriv(GLenum pname, const GLint *params)
  (define-cdecl void glPointParameteriv (unsigned-int void*))
  ;; void glBlendColor(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
  (define-cdecl void glBlendColor (float float float float))
  ;; void glBlendEquation(GLenum mode)
  (define-cdecl void glBlendEquation (unsigned-int))
  ;; void glGenQueries(GLsizei n, GLuint *ids)
  (define-cdecl void glGenQueries (int void*))
  ;; void glDeleteQueries(GLsizei n, const GLuint *ids)
  (define-cdecl void glDeleteQueries (int void*))
  ;; GLboolean glIsQuery(GLuint id)
  (define-cdecl uint8_t glIsQuery (unsigned-int))
  ;; void glBeginQuery(GLenum target, GLuint id)
  (define-cdecl void glBeginQuery (unsigned-int unsigned-int))
  ;; void glEndQuery(GLenum target)
  (define-cdecl void glEndQuery (unsigned-int))
  ;; void glGetQueryiv(GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetQueryiv (unsigned-int unsigned-int void*))
  ;; void glGetQueryObjectiv(GLuint id, GLenum pname, GLint *params)
  (define-cdecl void glGetQueryObjectiv (unsigned-int unsigned-int void*))
  ;; void glGetQueryObjectuiv(GLuint id, GLenum pname, GLuint *params)
  (define-cdecl void glGetQueryObjectuiv (unsigned-int unsigned-int void*))
  ;; void glBindBuffer(GLenum target, GLuint buffer)
  (define-cdecl void glBindBuffer (unsigned-int unsigned-int))
  ;; void glDeleteBuffers(GLsizei n, const GLuint *buffers)
  (define-cdecl void glDeleteBuffers (int void*))
  ;; void glGenBuffers(GLsizei n, GLuint *buffers)
  (define-cdecl void glGenBuffers (int void*))
  ;; GLboolean glIsBuffer(GLuint buffer)
  (define-cdecl uint8_t glIsBuffer (unsigned-int))
  ;; void glBufferData(GLenum target, GLsizeiptr size, const void *data, GLenum usage)
  (define-cdecl void glBufferData (unsigned-int int void* unsigned-int))
  ;; void glBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, const void *data)
  (define-cdecl void glBufferSubData (unsigned-int int int void*))
  ;; void glGetBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, void *data)
  (define-cdecl void glGetBufferSubData (unsigned-int int int void*))
  ;; GLboolean glUnmapBuffer(GLenum target)
  (define-cdecl uint8_t glUnmapBuffer (unsigned-int))
  ;; void glGetBufferParameteriv(GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetBufferParameteriv (unsigned-int unsigned-int void*))
  ;; void glGetBufferPointerv(GLenum target, GLenum pname, void **params)
  (define-cdecl void glGetBufferPointerv (unsigned-int unsigned-int void*))
  ;; void glBlendEquationSeparate(GLenum modeRGB, GLenum modeAlpha)
  (define-cdecl void glBlendEquationSeparate (unsigned-int unsigned-int))
  ;; void glDrawBuffers(GLsizei n, const GLenum *bufs)
  (define-cdecl void glDrawBuffers (int void*))
  ;; void glStencilOpSeparate(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass)
  (define-cdecl void glStencilOpSeparate (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glStencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask)
  (define-cdecl void glStencilFuncSeparate (unsigned-int unsigned-int int unsigned-int))
  ;; void glStencilMaskSeparate(GLenum face, GLuint mask)
  (define-cdecl void glStencilMaskSeparate (unsigned-int unsigned-int))
  ;; void glAttachShader(GLuint program, GLuint shader)
  (define-cdecl void glAttachShader (unsigned-int unsigned-int))
  ;; void glBindAttribLocation(GLuint program, GLuint index, const GLchar *name)
  (define-cdecl void glBindAttribLocation (unsigned-int unsigned-int void*))
  ;; void glCompileShader(GLuint shader)
  (define-cdecl void glCompileShader (unsigned-int))
  ;; GLuint glCreateProgram(void)
  (define-cdecl unsigned-int glCreateProgram ())
  ;; GLuint glCreateShader(GLenum type)
  (define-cdecl unsigned-int glCreateShader (unsigned-int))
  ;; void glDeleteProgram(GLuint program)
  (define-cdecl void glDeleteProgram (unsigned-int))
  ;; void glDeleteShader(GLuint shader)
  (define-cdecl void glDeleteShader (unsigned-int))
  ;; void glDetachShader(GLuint program, GLuint shader)
  (define-cdecl void glDetachShader (unsigned-int unsigned-int))
  ;; void glDisableVertexAttribArray(GLuint index)
  (define-cdecl void glDisableVertexAttribArray (unsigned-int))
  ;; void glEnableVertexAttribArray(GLuint index)
  (define-cdecl void glEnableVertexAttribArray (unsigned-int))
  ;; void glGetActiveAttrib(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name)
  (define-cdecl void glGetActiveAttrib (unsigned-int unsigned-int int void* void* void* void*))
  ;; void glGetActiveUniform(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name)
  (define-cdecl void glGetActiveUniform (unsigned-int unsigned-int int void* void* void* void*))
  ;; void glGetAttachedShaders(GLuint program, GLsizei maxCount, GLsizei *count, GLuint *shaders)
  (define-cdecl void glGetAttachedShaders (unsigned-int int void* void*))
  ;; GLint glGetAttribLocation(GLuint program, const GLchar *name)
  (define-cdecl int glGetAttribLocation (unsigned-int void*))
  ;; void glGetProgramiv(GLuint program, GLenum pname, GLint *params)
  (define-cdecl void glGetProgramiv (unsigned-int unsigned-int void*))
  ;; void glGetProgramInfoLog(GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog)
  (define-cdecl void glGetProgramInfoLog (unsigned-int int void* void*))
  ;; void glGetShaderiv(GLuint shader, GLenum pname, GLint *params)
  (define-cdecl void glGetShaderiv (unsigned-int unsigned-int void*))
  ;; void glGetShaderInfoLog(GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog)
  (define-cdecl void glGetShaderInfoLog (unsigned-int int void* void*))
  ;; void glGetShaderSource(GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *source)
  (define-cdecl void glGetShaderSource (unsigned-int int void* void*))
  ;; GLint glGetUniformLocation(GLuint program, const GLchar *name)
  (define-cdecl int glGetUniformLocation (unsigned-int void*))
  ;; void glGetUniformfv(GLuint program, GLint location, GLfloat *params)
  (define-cdecl void glGetUniformfv (unsigned-int int void*))
  ;; void glGetUniformiv(GLuint program, GLint location, GLint *params)
  (define-cdecl void glGetUniformiv (unsigned-int int void*))
  ;; void glGetVertexAttribdv(GLuint index, GLenum pname, GLdouble *params)
  (define-cdecl void glGetVertexAttribdv (unsigned-int unsigned-int void*))
  ;; void glGetVertexAttribfv(GLuint index, GLenum pname, GLfloat *params)
  (define-cdecl void glGetVertexAttribfv (unsigned-int unsigned-int void*))
  ;; void glGetVertexAttribiv(GLuint index, GLenum pname, GLint *params)
  (define-cdecl void glGetVertexAttribiv (unsigned-int unsigned-int void*))
  ;; void glGetVertexAttribPointerv(GLuint index, GLenum pname, void **pointer)
  (define-cdecl void glGetVertexAttribPointerv (unsigned-int unsigned-int void*))
  ;; GLboolean glIsProgram(GLuint program)
  (define-cdecl uint8_t glIsProgram (unsigned-int))
  ;; GLboolean glIsShader(GLuint shader)
  (define-cdecl uint8_t glIsShader (unsigned-int))
  ;; void glLinkProgram(GLuint program)
  (define-cdecl void glLinkProgram (unsigned-int))
  ;; void glShaderSource(GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length)
  (define-cdecl void glShaderSource (unsigned-int int void* void*))
  ;; void glUseProgram(GLuint program)
  (define-cdecl void glUseProgram (unsigned-int))
  ;; void glUniform1f(GLint location, GLfloat v0)
  (define-cdecl void glUniform1f (int float))
  ;; void glUniform2f(GLint location, GLfloat v0, GLfloat v1)
  (define-cdecl void glUniform2f (int float float))
  ;; void glUniform3f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2)
  (define-cdecl void glUniform3f (int float float float))
  ;; void glUniform4f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)
  (define-cdecl void glUniform4f (int float float float float))
  ;; void glUniform1i(GLint location, GLint v0)
  (define-cdecl void glUniform1i (int int))
  ;; void glUniform2i(GLint location, GLint v0, GLint v1)
  (define-cdecl void glUniform2i (int int int))
  ;; void glUniform3i(GLint location, GLint v0, GLint v1, GLint v2)
  (define-cdecl void glUniform3i (int int int int))
  ;; void glUniform4i(GLint location, GLint v0, GLint v1, GLint v2, GLint v3)
  (define-cdecl void glUniform4i (int int int int int))
  ;; void glUniform1fv(GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glUniform1fv (int int void*))
  ;; void glUniform2fv(GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glUniform2fv (int int void*))
  ;; void glUniform3fv(GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glUniform3fv (int int void*))
  ;; void glUniform4fv(GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glUniform4fv (int int void*))
  ;; void glUniform1iv(GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glUniform1iv (int int void*))
  ;; void glUniform2iv(GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glUniform2iv (int int void*))
  ;; void glUniform3iv(GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glUniform3iv (int int void*))
  ;; void glUniform4iv(GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glUniform4iv (int int void*))
  ;; void glUniformMatrix2fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix2fv (int int uint8_t void*))
  ;; void glUniformMatrix3fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix3fv (int int uint8_t void*))
  ;; void glUniformMatrix4fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix4fv (int int uint8_t void*))
  ;; void glValidateProgram(GLuint program)
  (define-cdecl void glValidateProgram (unsigned-int))
  ;; void glVertexAttrib1d(GLuint index, GLdouble x)
  (define-cdecl void glVertexAttrib1d (unsigned-int double))
  ;; void glVertexAttrib1dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttrib1dv (unsigned-int void*))
  ;; void glVertexAttrib1f(GLuint index, GLfloat x)
  (define-cdecl void glVertexAttrib1f (unsigned-int float))
  ;; void glVertexAttrib1fv(GLuint index, const GLfloat *v)
  (define-cdecl void glVertexAttrib1fv (unsigned-int void*))
  ;; void glVertexAttrib1s(GLuint index, GLshort x)
  (define-cdecl void glVertexAttrib1s (unsigned-int short))
  ;; void glVertexAttrib1sv(GLuint index, const GLshort *v)
  (define-cdecl void glVertexAttrib1sv (unsigned-int void*))
  ;; void glVertexAttrib2d(GLuint index, GLdouble x, GLdouble y)
  (define-cdecl void glVertexAttrib2d (unsigned-int double double))
  ;; void glVertexAttrib2dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttrib2dv (unsigned-int void*))
  ;; void glVertexAttrib2f(GLuint index, GLfloat x, GLfloat y)
  (define-cdecl void glVertexAttrib2f (unsigned-int float float))
  ;; void glVertexAttrib2fv(GLuint index, const GLfloat *v)
  (define-cdecl void glVertexAttrib2fv (unsigned-int void*))
  ;; void glVertexAttrib2s(GLuint index, GLshort x, GLshort y)
  (define-cdecl void glVertexAttrib2s (unsigned-int short short))
  ;; void glVertexAttrib2sv(GLuint index, const GLshort *v)
  (define-cdecl void glVertexAttrib2sv (unsigned-int void*))
  ;; void glVertexAttrib3d(GLuint index, GLdouble x, GLdouble y, GLdouble z)
  (define-cdecl void glVertexAttrib3d (unsigned-int double double double))
  ;; void glVertexAttrib3dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttrib3dv (unsigned-int void*))
  ;; void glVertexAttrib3f(GLuint index, GLfloat x, GLfloat y, GLfloat z)
  (define-cdecl void glVertexAttrib3f (unsigned-int float float float))
  ;; void glVertexAttrib3fv(GLuint index, const GLfloat *v)
  (define-cdecl void glVertexAttrib3fv (unsigned-int void*))
  ;; void glVertexAttrib3s(GLuint index, GLshort x, GLshort y, GLshort z)
  (define-cdecl void glVertexAttrib3s (unsigned-int short short short))
  ;; void glVertexAttrib3sv(GLuint index, const GLshort *v)
  (define-cdecl void glVertexAttrib3sv (unsigned-int void*))
  ;; void glVertexAttrib4Nbv(GLuint index, const GLbyte *v)
  (define-cdecl void glVertexAttrib4Nbv (unsigned-int void*))
  ;; void glVertexAttrib4Niv(GLuint index, const GLint *v)
  (define-cdecl void glVertexAttrib4Niv (unsigned-int void*))
  ;; void glVertexAttrib4Nsv(GLuint index, const GLshort *v)
  (define-cdecl void glVertexAttrib4Nsv (unsigned-int void*))
  ;; void glVertexAttrib4Nub(GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w)
  (define-cdecl void glVertexAttrib4Nub (unsigned-int uint8_t uint8_t uint8_t uint8_t))
  ;; void glVertexAttrib4Nubv(GLuint index, const GLubyte *v)
  (define-cdecl void glVertexAttrib4Nubv (unsigned-int void*))
  ;; void glVertexAttrib4Nuiv(GLuint index, const GLuint *v)
  (define-cdecl void glVertexAttrib4Nuiv (unsigned-int void*))
  ;; void glVertexAttrib4Nusv(GLuint index, const GLushort *v)
  (define-cdecl void glVertexAttrib4Nusv (unsigned-int void*))
  ;; void glVertexAttrib4bv(GLuint index, const GLbyte *v)
  (define-cdecl void glVertexAttrib4bv (unsigned-int void*))
  ;; void glVertexAttrib4d(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
  (define-cdecl void glVertexAttrib4d (unsigned-int double double double double))
  ;; void glVertexAttrib4dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttrib4dv (unsigned-int void*))
  ;; void glVertexAttrib4f(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
  (define-cdecl void glVertexAttrib4f (unsigned-int float float float float))
  ;; void glVertexAttrib4fv(GLuint index, const GLfloat *v)
  (define-cdecl void glVertexAttrib4fv (unsigned-int void*))
  ;; void glVertexAttrib4iv(GLuint index, const GLint *v)
  (define-cdecl void glVertexAttrib4iv (unsigned-int void*))
  ;; void glVertexAttrib4s(GLuint index, GLshort x, GLshort y, GLshort z, GLshort w)
  (define-cdecl void glVertexAttrib4s (unsigned-int short short short short))
  ;; void glVertexAttrib4sv(GLuint index, const GLshort *v)
  (define-cdecl void glVertexAttrib4sv (unsigned-int void*))
  ;; void glVertexAttrib4ubv(GLuint index, const GLubyte *v)
  (define-cdecl void glVertexAttrib4ubv (unsigned-int void*))
  ;; void glVertexAttrib4uiv(GLuint index, const GLuint *v)
  (define-cdecl void glVertexAttrib4uiv (unsigned-int void*))
  ;; void glVertexAttrib4usv(GLuint index, const GLushort *v)
  (define-cdecl void glVertexAttrib4usv (unsigned-int void*))
  ;; void glVertexAttribPointer(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer)
  (define-cdecl void glVertexAttribPointer (unsigned-int int unsigned-int uint8_t int void*))
  ;; void glUniformMatrix2x3fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix2x3fv (int int uint8_t void*))
  ;; void glUniformMatrix3x2fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix3x2fv (int int uint8_t void*))
  ;; void glUniformMatrix2x4fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix2x4fv (int int uint8_t void*))
  ;; void glUniformMatrix4x2fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix4x2fv (int int uint8_t void*))
  ;; void glUniformMatrix3x4fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix3x4fv (int int uint8_t void*))
  ;; void glUniformMatrix4x3fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glUniformMatrix4x3fv (int int uint8_t void*))
  ;; void glColorMaski(GLuint index, GLboolean r, GLboolean g, GLboolean b, GLboolean a)
  (define-cdecl void glColorMaski (unsigned-int uint8_t uint8_t uint8_t uint8_t))
  ;; void glEnablei(GLenum target, GLuint index)
  (define-cdecl void glEnablei (unsigned-int unsigned-int))
  ;; void glDisablei(GLenum target, GLuint index)
  (define-cdecl void glDisablei (unsigned-int unsigned-int))
  ;; GLboolean glIsEnabledi(GLenum target, GLuint index)
  (define-cdecl uint8_t glIsEnabledi (unsigned-int unsigned-int))
  ;; void glBeginTransformFeedback(GLenum primitiveMode)
  (define-cdecl void glBeginTransformFeedback (unsigned-int))
  ;; void glEndTransformFeedback(void)
  (define-cdecl void glEndTransformFeedback ())
  ;; void glBindBufferRange(GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size)
  (define-cdecl void glBindBufferRange (unsigned-int unsigned-int unsigned-int int int))
  ;; void glBindBufferBase(GLenum target, GLuint index, GLuint buffer)
  (define-cdecl void glBindBufferBase (unsigned-int unsigned-int unsigned-int))
  ;; void glTransformFeedbackVaryings(GLuint program, GLsizei count, const GLchar *const*varyings, GLenum bufferMode)
  (define-cdecl void glTransformFeedbackVaryings (unsigned-int int void* unsigned-int))
  ;; void glGetTransformFeedbackVarying(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLsizei *size, GLenum *type, GLchar *name)
  (define-cdecl void glGetTransformFeedbackVarying (unsigned-int unsigned-int int void* void* void* void*))
  ;; void glClampColor(GLenum target, GLenum clamp)
  (define-cdecl void glClampColor (unsigned-int unsigned-int))
  ;; void glBeginConditionalRender(GLuint id, GLenum mode)
  (define-cdecl void glBeginConditionalRender (unsigned-int unsigned-int))
  ;; void glEndConditionalRender(void)
  (define-cdecl void glEndConditionalRender ())
  ;; void glVertexAttribIPointer(GLuint index, GLint size, GLenum type, GLsizei stride, const void *pointer)
  (define-cdecl void glVertexAttribIPointer (unsigned-int int unsigned-int int void*))
  ;; void glGetVertexAttribIiv(GLuint index, GLenum pname, GLint *params)
  (define-cdecl void glGetVertexAttribIiv (unsigned-int unsigned-int void*))
  ;; void glGetVertexAttribIuiv(GLuint index, GLenum pname, GLuint *params)
  (define-cdecl void glGetVertexAttribIuiv (unsigned-int unsigned-int void*))
  ;; void glVertexAttribI1i(GLuint index, GLint x)
  (define-cdecl void glVertexAttribI1i (unsigned-int int))
  ;; void glVertexAttribI2i(GLuint index, GLint x, GLint y)
  (define-cdecl void glVertexAttribI2i (unsigned-int int int))
  ;; void glVertexAttribI3i(GLuint index, GLint x, GLint y, GLint z)
  (define-cdecl void glVertexAttribI3i (unsigned-int int int int))
  ;; void glVertexAttribI4i(GLuint index, GLint x, GLint y, GLint z, GLint w)
  (define-cdecl void glVertexAttribI4i (unsigned-int int int int int))
  ;; void glVertexAttribI1ui(GLuint index, GLuint x)
  (define-cdecl void glVertexAttribI1ui (unsigned-int unsigned-int))
  ;; void glVertexAttribI2ui(GLuint index, GLuint x, GLuint y)
  (define-cdecl void glVertexAttribI2ui (unsigned-int unsigned-int unsigned-int))
  ;; void glVertexAttribI3ui(GLuint index, GLuint x, GLuint y, GLuint z)
  (define-cdecl void glVertexAttribI3ui (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glVertexAttribI4ui(GLuint index, GLuint x, GLuint y, GLuint z, GLuint w)
  (define-cdecl void glVertexAttribI4ui (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glVertexAttribI1iv(GLuint index, const GLint *v)
  (define-cdecl void glVertexAttribI1iv (unsigned-int void*))
  ;; void glVertexAttribI2iv(GLuint index, const GLint *v)
  (define-cdecl void glVertexAttribI2iv (unsigned-int void*))
  ;; void glVertexAttribI3iv(GLuint index, const GLint *v)
  (define-cdecl void glVertexAttribI3iv (unsigned-int void*))
  ;; void glVertexAttribI4iv(GLuint index, const GLint *v)
  (define-cdecl void glVertexAttribI4iv (unsigned-int void*))
  ;; void glVertexAttribI1uiv(GLuint index, const GLuint *v)
  (define-cdecl void glVertexAttribI1uiv (unsigned-int void*))
  ;; void glVertexAttribI2uiv(GLuint index, const GLuint *v)
  (define-cdecl void glVertexAttribI2uiv (unsigned-int void*))
  ;; void glVertexAttribI3uiv(GLuint index, const GLuint *v)
  (define-cdecl void glVertexAttribI3uiv (unsigned-int void*))
  ;; void glVertexAttribI4uiv(GLuint index, const GLuint *v)
  (define-cdecl void glVertexAttribI4uiv (unsigned-int void*))
  ;; void glVertexAttribI4bv(GLuint index, const GLbyte *v)
  (define-cdecl void glVertexAttribI4bv (unsigned-int void*))
  ;; void glVertexAttribI4sv(GLuint index, const GLshort *v)
  (define-cdecl void glVertexAttribI4sv (unsigned-int void*))
  ;; void glVertexAttribI4ubv(GLuint index, const GLubyte *v)
  (define-cdecl void glVertexAttribI4ubv (unsigned-int void*))
  ;; void glVertexAttribI4usv(GLuint index, const GLushort *v)
  (define-cdecl void glVertexAttribI4usv (unsigned-int void*))
  ;; void glGetUniformuiv(GLuint program, GLint location, GLuint *params)
  (define-cdecl void glGetUniformuiv (unsigned-int int void*))
  ;; void glBindFragDataLocation(GLuint program, GLuint color, const GLchar *name)
  (define-cdecl void glBindFragDataLocation (unsigned-int unsigned-int void*))
  ;; GLint glGetFragDataLocation(GLuint program, const GLchar *name)
  (define-cdecl int glGetFragDataLocation (unsigned-int void*))
  ;; void glUniform1ui(GLint location, GLuint v0)
  (define-cdecl void glUniform1ui (int unsigned-int))
  ;; void glUniform2ui(GLint location, GLuint v0, GLuint v1)
  (define-cdecl void glUniform2ui (int unsigned-int unsigned-int))
  ;; void glUniform3ui(GLint location, GLuint v0, GLuint v1, GLuint v2)
  (define-cdecl void glUniform3ui (int unsigned-int unsigned-int unsigned-int))
  ;; void glUniform4ui(GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3)
  (define-cdecl void glUniform4ui (int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glUniform1uiv(GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glUniform1uiv (int int void*))
  ;; void glUniform2uiv(GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glUniform2uiv (int int void*))
  ;; void glUniform3uiv(GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glUniform3uiv (int int void*))
  ;; void glUniform4uiv(GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glUniform4uiv (int int void*))
  ;; void glTexParameterIiv(GLenum target, GLenum pname, const GLint *params)
  (define-cdecl void glTexParameterIiv (unsigned-int unsigned-int void*))
  ;; void glTexParameterIuiv(GLenum target, GLenum pname, const GLuint *params)
  (define-cdecl void glTexParameterIuiv (unsigned-int unsigned-int void*))
  ;; void glGetTexParameterIiv(GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetTexParameterIiv (unsigned-int unsigned-int void*))
  ;; void glGetTexParameterIuiv(GLenum target, GLenum pname, GLuint *params)
  (define-cdecl void glGetTexParameterIuiv (unsigned-int unsigned-int void*))
  ;; void glClearBufferiv(GLenum buffer, GLint drawbuffer, const GLint *value)
  (define-cdecl void glClearBufferiv (unsigned-int int void*))
  ;; void glClearBufferuiv(GLenum buffer, GLint drawbuffer, const GLuint *value)
  (define-cdecl void glClearBufferuiv (unsigned-int int void*))
  ;; void glClearBufferfv(GLenum buffer, GLint drawbuffer, const GLfloat *value)
  (define-cdecl void glClearBufferfv (unsigned-int int void*))
  ;; void glClearBufferfi(GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil)
  (define-cdecl void glClearBufferfi (unsigned-int int float int))
  ;; GLboolean glIsRenderbuffer(GLuint renderbuffer)
  (define-cdecl uint8_t glIsRenderbuffer (unsigned-int))
  ;; void glBindRenderbuffer(GLenum target, GLuint renderbuffer)
  (define-cdecl void glBindRenderbuffer (unsigned-int unsigned-int))
  ;; void glDeleteRenderbuffers(GLsizei n, const GLuint *renderbuffers)
  (define-cdecl void glDeleteRenderbuffers (int void*))
  ;; void glGenRenderbuffers(GLsizei n, GLuint *renderbuffers)
  (define-cdecl void glGenRenderbuffers (int void*))
  ;; void glRenderbufferStorage(GLenum target, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glRenderbufferStorage (unsigned-int unsigned-int int int))
  ;; void glGetRenderbufferParameteriv(GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetRenderbufferParameteriv (unsigned-int unsigned-int void*))
  ;; GLboolean glIsFramebuffer(GLuint framebuffer)
  (define-cdecl uint8_t glIsFramebuffer (unsigned-int))
  ;; void glBindFramebuffer(GLenum target, GLuint framebuffer)
  (define-cdecl void glBindFramebuffer (unsigned-int unsigned-int))
  ;; void glDeleteFramebuffers(GLsizei n, const GLuint *framebuffers)
  (define-cdecl void glDeleteFramebuffers (int void*))
  ;; void glGenFramebuffers(GLsizei n, GLuint *framebuffers)
  (define-cdecl void glGenFramebuffers (int void*))
  ;; GLenum glCheckFramebufferStatus(GLenum target)
  (define-cdecl unsigned-int glCheckFramebufferStatus (unsigned-int))
  ;; void glFramebufferTexture1D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)
  (define-cdecl void glFramebufferTexture1D (unsigned-int unsigned-int unsigned-int unsigned-int int))
  ;; void glFramebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)
  (define-cdecl void glFramebufferTexture2D (unsigned-int unsigned-int unsigned-int unsigned-int int))
  ;; void glFramebufferTexture3D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level, GLint zoffset)
  (define-cdecl void glFramebufferTexture3D (unsigned-int unsigned-int unsigned-int unsigned-int int int))
  ;; void glFramebufferRenderbuffer(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)
  (define-cdecl void glFramebufferRenderbuffer (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glGetFramebufferAttachmentParameteriv(GLenum target, GLenum attachment, GLenum pname, GLint *params)
  (define-cdecl void glGetFramebufferAttachmentParameteriv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGenerateMipmap(GLenum target)
  (define-cdecl void glGenerateMipmap (unsigned-int))
  ;; void glBlitFramebuffer(GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0, GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter)
  (define-cdecl void glBlitFramebuffer (int int int int int int int int unsigned-int unsigned-int))
  ;; void glRenderbufferStorageMultisample(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glRenderbufferStorageMultisample (unsigned-int int unsigned-int int int))
  ;; void glFramebufferTextureLayer(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer)
  (define-cdecl void glFramebufferTextureLayer (unsigned-int unsigned-int unsigned-int int int))
  ;; void glFlushMappedBufferRange(GLenum target, GLintptr offset, GLsizeiptr length)
  (define-cdecl void glFlushMappedBufferRange (unsigned-int int int))
  ;; void glBindVertexArray(GLuint array)
  (define-cdecl void glBindVertexArray (unsigned-int))
  ;; void glDeleteVertexArrays(GLsizei n, const GLuint *arrays)
  (define-cdecl void glDeleteVertexArrays (int void*))
  ;; void glGenVertexArrays(GLsizei n, GLuint *arrays)
  (define-cdecl void glGenVertexArrays (int void*))
  ;; GLboolean glIsVertexArray(GLuint array)
  (define-cdecl uint8_t glIsVertexArray (unsigned-int))
  ;; void glDrawArraysInstanced(GLenum mode, GLint first, GLsizei count, GLsizei instancecount)
  (define-cdecl void glDrawArraysInstanced (unsigned-int int int int))
  ;; void glDrawElementsInstanced(GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei instancecount)
  (define-cdecl void glDrawElementsInstanced (unsigned-int int unsigned-int void* int))
  ;; void glTexBuffer(GLenum target, GLenum internalformat, GLuint buffer)
  (define-cdecl void glTexBuffer (unsigned-int unsigned-int unsigned-int))
  ;; void glPrimitiveRestartIndex(GLuint index)
  (define-cdecl void glPrimitiveRestartIndex (unsigned-int))
  ;; void glCopyBufferSubData(GLenum readTarget, GLenum writeTarget, GLintptr readOffset, GLintptr writeOffset, GLsizeiptr size)
  (define-cdecl void glCopyBufferSubData (unsigned-int unsigned-int int int int))
  ;; void glGetUniformIndices(GLuint program, GLsizei uniformCount, const GLchar *const*uniformNames, GLuint *uniformIndices)
  (define-cdecl void glGetUniformIndices (unsigned-int int void* void*))
  ;; void glGetActiveUniformsiv(GLuint program, GLsizei uniformCount, const GLuint *uniformIndices, GLenum pname, GLint *params)
  (define-cdecl void glGetActiveUniformsiv (unsigned-int int void* unsigned-int void*))
  ;; void glGetActiveUniformName(GLuint program, GLuint uniformIndex, GLsizei bufSize, GLsizei *length, GLchar *uniformName)
  (define-cdecl void glGetActiveUniformName (unsigned-int unsigned-int int void* void*))
  ;; GLuint glGetUniformBlockIndex(GLuint program, const GLchar *uniformBlockName)
  (define-cdecl unsigned-int glGetUniformBlockIndex (unsigned-int void*))
  ;; void glGetActiveUniformBlockiv(GLuint program, GLuint uniformBlockIndex, GLenum pname, GLint *params)
  (define-cdecl void glGetActiveUniformBlockiv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetActiveUniformBlockName(GLuint program, GLuint uniformBlockIndex, GLsizei bufSize, GLsizei *length, GLchar *uniformBlockName)
  (define-cdecl void glGetActiveUniformBlockName (unsigned-int unsigned-int int void* void*))
  ;; void glUniformBlockBinding(GLuint program, GLuint uniformBlockIndex, GLuint uniformBlockBinding)
  (define-cdecl void glUniformBlockBinding (unsigned-int unsigned-int unsigned-int))
  ;; void glDrawElementsBaseVertex(GLenum mode, GLsizei count, GLenum type, const void *indices, GLint basevertex)
  (define-cdecl void glDrawElementsBaseVertex (unsigned-int int unsigned-int void* int))
  ;; void glDrawRangeElementsBaseVertex(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices, GLint basevertex)
  (define-cdecl void glDrawRangeElementsBaseVertex (unsigned-int unsigned-int unsigned-int int unsigned-int void* int))
  ;; void glDrawElementsInstancedBaseVertex(GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei instancecount, GLint basevertex)
  (define-cdecl void glDrawElementsInstancedBaseVertex (unsigned-int int unsigned-int void* int int))
  ;; void glMultiDrawElementsBaseVertex(GLenum mode, const GLsizei *count, GLenum type, const void *const*indices, GLsizei drawcount, const GLint *basevertex)
  (define-cdecl void glMultiDrawElementsBaseVertex (unsigned-int void* unsigned-int void* int void*))
  ;; void glProvokingVertex(GLenum mode)
  (define-cdecl void glProvokingVertex (unsigned-int))
  ;; GLsync glFenceSync(GLenum condition, GLbitfield flags)
  (define-cdecl void* glFenceSync (unsigned-int unsigned-int))
  ;; GLboolean glIsSync(GLsync sync)
  (define-cdecl uint8_t glIsSync (void*))
  ;; void glDeleteSync(GLsync sync)
  (define-cdecl void glDeleteSync (void*))
  ;; GLenum glClientWaitSync(GLsync sync, GLbitfield flags, GLuint64 timeout)
  (define-cdecl unsigned-int glClientWaitSync (void* unsigned-int unsigned-int))
  ;; void glWaitSync(GLsync sync, GLbitfield flags, GLuint64 timeout)
  (define-cdecl void glWaitSync (void* unsigned-int unsigned-int))
  ;; void glGetInteger64v(GLenum pname, GLint64 *data)
  (define-cdecl void glGetInteger64v (unsigned-int void*))
  ;; void glGetSynciv(GLsync sync, GLenum pname, GLsizei count, GLsizei *length, GLint *values)
  (define-cdecl void glGetSynciv (void* unsigned-int int void* void*))
  ;; void glGetBufferParameteri64v(GLenum target, GLenum pname, GLint64 *params)
  (define-cdecl void glGetBufferParameteri64v (unsigned-int unsigned-int void*))
  ;; void glFramebufferTexture(GLenum target, GLenum attachment, GLuint texture, GLint level)
  (define-cdecl void glFramebufferTexture (unsigned-int unsigned-int unsigned-int int))
  ;; void glTexImage2DMultisample(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLboolean fixedsamplelocations)
  (define-cdecl void glTexImage2DMultisample (unsigned-int int unsigned-int int int uint8_t))
  ;; void glTexImage3DMultisample(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean fixedsamplelocations)
  (define-cdecl void glTexImage3DMultisample (unsigned-int int unsigned-int int int int uint8_t))
  ;; void glGetMultisamplefv(GLenum pname, GLuint index, GLfloat *val)
  (define-cdecl void glGetMultisamplefv (unsigned-int unsigned-int void*))
  ;; void glSampleMaski(GLuint maskNumber, GLbitfield mask)
  (define-cdecl void glSampleMaski (unsigned-int unsigned-int))
  ;; void glBindFragDataLocationIndexed(GLuint program, GLuint colorNumber, GLuint index, const GLchar *name)
  (define-cdecl void glBindFragDataLocationIndexed (unsigned-int unsigned-int unsigned-int void*))
  ;; GLint glGetFragDataIndex(GLuint program, const GLchar *name)
  (define-cdecl int glGetFragDataIndex (unsigned-int void*))
  ;; void glGenSamplers(GLsizei count, GLuint *samplers)
  (define-cdecl void glGenSamplers (int void*))
  ;; void glDeleteSamplers(GLsizei count, const GLuint *samplers)
  (define-cdecl void glDeleteSamplers (int void*))
  ;; GLboolean glIsSampler(GLuint sampler)
  (define-cdecl uint8_t glIsSampler (unsigned-int))
  ;; void glBindSampler(GLuint unit, GLuint sampler)
  (define-cdecl void glBindSampler (unsigned-int unsigned-int))
  ;; void glSamplerParameteri(GLuint sampler, GLenum pname, GLint param)
  (define-cdecl void glSamplerParameteri (unsigned-int unsigned-int int))
  ;; void glSamplerParameteriv(GLuint sampler, GLenum pname, const GLint *param)
  (define-cdecl void glSamplerParameteriv (unsigned-int unsigned-int void*))
  ;; void glSamplerParameterf(GLuint sampler, GLenum pname, GLfloat param)
  (define-cdecl void glSamplerParameterf (unsigned-int unsigned-int float))
  ;; void glSamplerParameterfv(GLuint sampler, GLenum pname, const GLfloat *param)
  (define-cdecl void glSamplerParameterfv (unsigned-int unsigned-int void*))
  ;; void glSamplerParameterIiv(GLuint sampler, GLenum pname, const GLint *param)
  (define-cdecl void glSamplerParameterIiv (unsigned-int unsigned-int void*))
  ;; void glSamplerParameterIuiv(GLuint sampler, GLenum pname, const GLuint *param)
  (define-cdecl void glSamplerParameterIuiv (unsigned-int unsigned-int void*))
  ;; void glGetSamplerParameteriv(GLuint sampler, GLenum pname, GLint *params)
  (define-cdecl void glGetSamplerParameteriv (unsigned-int unsigned-int void*))
  ;; void glGetSamplerParameterIiv(GLuint sampler, GLenum pname, GLint *params)
  (define-cdecl void glGetSamplerParameterIiv (unsigned-int unsigned-int void*))
  ;; void glGetSamplerParameterfv(GLuint sampler, GLenum pname, GLfloat *params)
  (define-cdecl void glGetSamplerParameterfv (unsigned-int unsigned-int void*))
  ;; void glGetSamplerParameterIuiv(GLuint sampler, GLenum pname, GLuint *params)
  (define-cdecl void glGetSamplerParameterIuiv (unsigned-int unsigned-int void*))
  ;; void glQueryCounter(GLuint id, GLenum target)
  (define-cdecl void glQueryCounter (unsigned-int unsigned-int))
  ;; void glGetQueryObjecti64v(GLuint id, GLenum pname, GLint64 *params)
  (define-cdecl void glGetQueryObjecti64v (unsigned-int unsigned-int void*))
  ;; void glGetQueryObjectui64v(GLuint id, GLenum pname, GLuint64 *params)
  (define-cdecl void glGetQueryObjectui64v (unsigned-int unsigned-int void*))
  ;; void glVertexAttribDivisor(GLuint index, GLuint divisor)
  (define-cdecl void glVertexAttribDivisor (unsigned-int unsigned-int))
  ;; void glVertexAttribP1ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)
  (define-cdecl void glVertexAttribP1ui (unsigned-int unsigned-int uint8_t unsigned-int))
  ;; void glVertexAttribP1uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)
  (define-cdecl void glVertexAttribP1uiv (unsigned-int unsigned-int uint8_t void*))
  ;; void glVertexAttribP2ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)
  (define-cdecl void glVertexAttribP2ui (unsigned-int unsigned-int uint8_t unsigned-int))
  ;; void glVertexAttribP2uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)
  (define-cdecl void glVertexAttribP2uiv (unsigned-int unsigned-int uint8_t void*))
  ;; void glVertexAttribP3ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)
  (define-cdecl void glVertexAttribP3ui (unsigned-int unsigned-int uint8_t unsigned-int))
  ;; void glVertexAttribP3uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)
  (define-cdecl void glVertexAttribP3uiv (unsigned-int unsigned-int uint8_t void*))
  ;; void glVertexAttribP4ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)
  (define-cdecl void glVertexAttribP4ui (unsigned-int unsigned-int uint8_t unsigned-int))
  ;; void glVertexAttribP4uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)
  (define-cdecl void glVertexAttribP4uiv (unsigned-int unsigned-int uint8_t void*))
  ;; void glMinSampleShading(GLfloat value)
  (define-cdecl void glMinSampleShading (float))
  ;; void glBlendEquationi(GLuint buf, GLenum mode)
  (define-cdecl void glBlendEquationi (unsigned-int unsigned-int))
  ;; void glBlendEquationSeparatei(GLuint buf, GLenum modeRGB, GLenum modeAlpha)
  (define-cdecl void glBlendEquationSeparatei (unsigned-int unsigned-int unsigned-int))
  ;; void glBlendFunci(GLuint buf, GLenum src, GLenum dst)
  (define-cdecl void glBlendFunci (unsigned-int unsigned-int unsigned-int))
  ;; void glBlendFuncSeparatei(GLuint buf, GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha)
  (define-cdecl void glBlendFuncSeparatei (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glDrawArraysIndirect(GLenum mode, const void *indirect)
  (define-cdecl void glDrawArraysIndirect (unsigned-int void*))
  ;; void glDrawElementsIndirect(GLenum mode, GLenum type, const void *indirect)
  (define-cdecl void glDrawElementsIndirect (unsigned-int unsigned-int void*))
  ;; void glUniform1d(GLint location, GLdouble x)
  (define-cdecl void glUniform1d (int double))
  ;; void glUniform2d(GLint location, GLdouble x, GLdouble y)
  (define-cdecl void glUniform2d (int double double))
  ;; void glUniform3d(GLint location, GLdouble x, GLdouble y, GLdouble z)
  (define-cdecl void glUniform3d (int double double double))
  ;; void glUniform4d(GLint location, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
  (define-cdecl void glUniform4d (int double double double double))
  ;; void glUniform1dv(GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glUniform1dv (int int void*))
  ;; void glUniform2dv(GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glUniform2dv (int int void*))
  ;; void glUniform3dv(GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glUniform3dv (int int void*))
  ;; void glUniform4dv(GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glUniform4dv (int int void*))
  ;; void glUniformMatrix2dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix2dv (int int uint8_t void*))
  ;; void glUniformMatrix3dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix3dv (int int uint8_t void*))
  ;; void glUniformMatrix4dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix4dv (int int uint8_t void*))
  ;; void glUniformMatrix2x3dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix2x3dv (int int uint8_t void*))
  ;; void glUniformMatrix2x4dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix2x4dv (int int uint8_t void*))
  ;; void glUniformMatrix3x2dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix3x2dv (int int uint8_t void*))
  ;; void glUniformMatrix3x4dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix3x4dv (int int uint8_t void*))
  ;; void glUniformMatrix4x2dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix4x2dv (int int uint8_t void*))
  ;; void glUniformMatrix4x3dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glUniformMatrix4x3dv (int int uint8_t void*))
  ;; void glGetUniformdv(GLuint program, GLint location, GLdouble *params)
  (define-cdecl void glGetUniformdv (unsigned-int int void*))
  ;; GLint glGetSubroutineUniformLocation(GLuint program, GLenum shadertype, const GLchar *name)
  (define-cdecl int glGetSubroutineUniformLocation (unsigned-int unsigned-int void*))
  ;; GLuint glGetSubroutineIndex(GLuint program, GLenum shadertype, const GLchar *name)
  (define-cdecl unsigned-int glGetSubroutineIndex (unsigned-int unsigned-int void*))
  ;; void glGetActiveSubroutineUniformiv(GLuint program, GLenum shadertype, GLuint index, GLenum pname, GLint *values)
  (define-cdecl void glGetActiveSubroutineUniformiv (unsigned-int unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetActiveSubroutineUniformName(GLuint program, GLenum shadertype, GLuint index, GLsizei bufSize, GLsizei *length, GLchar *name)
  (define-cdecl void glGetActiveSubroutineUniformName (unsigned-int unsigned-int unsigned-int int void* void*))
  ;; void glGetActiveSubroutineName(GLuint program, GLenum shadertype, GLuint index, GLsizei bufSize, GLsizei *length, GLchar *name)
  (define-cdecl void glGetActiveSubroutineName (unsigned-int unsigned-int unsigned-int int void* void*))
  ;; void glUniformSubroutinesuiv(GLenum shadertype, GLsizei count, const GLuint *indices)
  (define-cdecl void glUniformSubroutinesuiv (unsigned-int int void*))
  ;; void glGetUniformSubroutineuiv(GLenum shadertype, GLint location, GLuint *params)
  (define-cdecl void glGetUniformSubroutineuiv (unsigned-int int void*))
  ;; void glGetProgramStageiv(GLuint program, GLenum shadertype, GLenum pname, GLint *values)
  (define-cdecl void glGetProgramStageiv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glPatchParameteri(GLenum pname, GLint value)
  (define-cdecl void glPatchParameteri (unsigned-int int))
  ;; void glPatchParameterfv(GLenum pname, const GLfloat *values)
  (define-cdecl void glPatchParameterfv (unsigned-int void*))
  ;; void glBindTransformFeedback(GLenum target, GLuint id)
  (define-cdecl void glBindTransformFeedback (unsigned-int unsigned-int))
  ;; void glDeleteTransformFeedbacks(GLsizei n, const GLuint *ids)
  (define-cdecl void glDeleteTransformFeedbacks (int void*))
  ;; void glGenTransformFeedbacks(GLsizei n, GLuint *ids)
  (define-cdecl void glGenTransformFeedbacks (int void*))
  ;; GLboolean glIsTransformFeedback(GLuint id)
  (define-cdecl uint8_t glIsTransformFeedback (unsigned-int))
  ;; void glPauseTransformFeedback(void)
  (define-cdecl void glPauseTransformFeedback ())
  ;; void glResumeTransformFeedback(void)
  (define-cdecl void glResumeTransformFeedback ())
  ;; void glDrawTransformFeedback(GLenum mode, GLuint id)
  (define-cdecl void glDrawTransformFeedback (unsigned-int unsigned-int))
  ;; void glDrawTransformFeedbackStream(GLenum mode, GLuint id, GLuint stream)
  (define-cdecl void glDrawTransformFeedbackStream (unsigned-int unsigned-int unsigned-int))
  ;; void glBeginQueryIndexed(GLenum target, GLuint index, GLuint id)
  (define-cdecl void glBeginQueryIndexed (unsigned-int unsigned-int unsigned-int))
  ;; void glEndQueryIndexed(GLenum target, GLuint index)
  (define-cdecl void glEndQueryIndexed (unsigned-int unsigned-int))
  ;; void glGetQueryIndexediv(GLenum target, GLuint index, GLenum pname, GLint *params)
  (define-cdecl void glGetQueryIndexediv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glReleaseShaderCompiler(void)
  (define-cdecl void glReleaseShaderCompiler ())
  ;; void glShaderBinary(GLsizei count, const GLuint *shaders, GLenum binaryformat, const void *binary, GLsizei length)
  (define-cdecl void glShaderBinary (int void* unsigned-int void* int))
  ;; void glGetShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype, GLint *range, GLint *precision)
  (define-cdecl void glGetShaderPrecisionFormat (unsigned-int unsigned-int void* void*))
  ;; void glDepthRangef(GLfloat n, GLfloat f)
  (define-cdecl void glDepthRangef (float float))
  ;; void glClearDepthf(GLfloat d)
  (define-cdecl void glClearDepthf (float))
  ;; void glGetProgramBinary(GLuint program, GLsizei bufSize, GLsizei *length, GLenum *binaryFormat, void *binary)
  (define-cdecl void glGetProgramBinary (unsigned-int int void* void* void*))
  ;; void glProgramBinary(GLuint program, GLenum binaryFormat, const void *binary, GLsizei length)
  (define-cdecl void glProgramBinary (unsigned-int unsigned-int void* int))
  ;; void glProgramParameteri(GLuint program, GLenum pname, GLint value)
  (define-cdecl void glProgramParameteri (unsigned-int unsigned-int int))
  ;; void glUseProgramStages(GLuint pipeline, GLbitfield stages, GLuint program)
  (define-cdecl void glUseProgramStages (unsigned-int unsigned-int unsigned-int))
  ;; void glActiveShaderProgram(GLuint pipeline, GLuint program)
  (define-cdecl void glActiveShaderProgram (unsigned-int unsigned-int))
  ;; GLuint glCreateShaderProgramv(GLenum type, GLsizei count, const GLchar *const*strings)
  (define-cdecl unsigned-int glCreateShaderProgramv (unsigned-int int void*))
  ;; void glBindProgramPipeline(GLuint pipeline)
  (define-cdecl void glBindProgramPipeline (unsigned-int))
  ;; void glDeleteProgramPipelines(GLsizei n, const GLuint *pipelines)
  (define-cdecl void glDeleteProgramPipelines (int void*))
  ;; void glGenProgramPipelines(GLsizei n, GLuint *pipelines)
  (define-cdecl void glGenProgramPipelines (int void*))
  ;; GLboolean glIsProgramPipeline(GLuint pipeline)
  (define-cdecl uint8_t glIsProgramPipeline (unsigned-int))
  ;; void glGetProgramPipelineiv(GLuint pipeline, GLenum pname, GLint *params)
  (define-cdecl void glGetProgramPipelineiv (unsigned-int unsigned-int void*))
  ;; void glProgramUniform1i(GLuint program, GLint location, GLint v0)
  (define-cdecl void glProgramUniform1i (unsigned-int int int))
  ;; void glProgramUniform1iv(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform1iv (unsigned-int int int void*))
  ;; void glProgramUniform1f(GLuint program, GLint location, GLfloat v0)
  (define-cdecl void glProgramUniform1f (unsigned-int int float))
  ;; void glProgramUniform1fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform1fv (unsigned-int int int void*))
  ;; void glProgramUniform1d(GLuint program, GLint location, GLdouble v0)
  (define-cdecl void glProgramUniform1d (unsigned-int int double))
  ;; void glProgramUniform1dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform1dv (unsigned-int int int void*))
  ;; void glProgramUniform1ui(GLuint program, GLint location, GLuint v0)
  (define-cdecl void glProgramUniform1ui (unsigned-int int unsigned-int))
  ;; void glProgramUniform1uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform1uiv (unsigned-int int int void*))
  ;; void glProgramUniform2i(GLuint program, GLint location, GLint v0, GLint v1)
  (define-cdecl void glProgramUniform2i (unsigned-int int int int))
  ;; void glProgramUniform2iv(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform2iv (unsigned-int int int void*))
  ;; void glProgramUniform2f(GLuint program, GLint location, GLfloat v0, GLfloat v1)
  (define-cdecl void glProgramUniform2f (unsigned-int int float float))
  ;; void glProgramUniform2fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform2fv (unsigned-int int int void*))
  ;; void glProgramUniform2d(GLuint program, GLint location, GLdouble v0, GLdouble v1)
  (define-cdecl void glProgramUniform2d (unsigned-int int double double))
  ;; void glProgramUniform2dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform2dv (unsigned-int int int void*))
  ;; void glProgramUniform2ui(GLuint program, GLint location, GLuint v0, GLuint v1)
  (define-cdecl void glProgramUniform2ui (unsigned-int int unsigned-int unsigned-int))
  ;; void glProgramUniform2uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform2uiv (unsigned-int int int void*))
  ;; void glProgramUniform3i(GLuint program, GLint location, GLint v0, GLint v1, GLint v2)
  (define-cdecl void glProgramUniform3i (unsigned-int int int int int))
  ;; void glProgramUniform3iv(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform3iv (unsigned-int int int void*))
  ;; void glProgramUniform3f(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2)
  (define-cdecl void glProgramUniform3f (unsigned-int int float float float))
  ;; void glProgramUniform3fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform3fv (unsigned-int int int void*))
  ;; void glProgramUniform3d(GLuint program, GLint location, GLdouble v0, GLdouble v1, GLdouble v2)
  (define-cdecl void glProgramUniform3d (unsigned-int int double double double))
  ;; void glProgramUniform3dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform3dv (unsigned-int int int void*))
  ;; void glProgramUniform3ui(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2)
  (define-cdecl void glProgramUniform3ui (unsigned-int int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform3uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform3uiv (unsigned-int int int void*))
  ;; void glProgramUniform4i(GLuint program, GLint location, GLint v0, GLint v1, GLint v2, GLint v3)
  (define-cdecl void glProgramUniform4i (unsigned-int int int int int int))
  ;; void glProgramUniform4iv(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform4iv (unsigned-int int int void*))
  ;; void glProgramUniform4f(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)
  (define-cdecl void glProgramUniform4f (unsigned-int int float float float float))
  ;; void glProgramUniform4fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform4fv (unsigned-int int int void*))
  ;; void glProgramUniform4d(GLuint program, GLint location, GLdouble v0, GLdouble v1, GLdouble v2, GLdouble v3)
  (define-cdecl void glProgramUniform4d (unsigned-int int double double double double))
  ;; void glProgramUniform4dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform4dv (unsigned-int int int void*))
  ;; void glProgramUniform4ui(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3)
  (define-cdecl void glProgramUniform4ui (unsigned-int int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform4uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform4uiv (unsigned-int int int void*))
  ;; void glProgramUniformMatrix2fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix2fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix3fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix4fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix2dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix3dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix4dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x3fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix2x3fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x2fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix3x2fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x4fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix2x4fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x2fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix4x2fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x4fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix3x4fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x3fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix4x3fv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x3dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix2x3dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x2dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix3x2dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x4dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix2x4dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x2dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix4x2dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x4dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix3x4dv (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x3dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix4x3dv (unsigned-int int int uint8_t void*))
  ;; void glValidateProgramPipeline(GLuint pipeline)
  (define-cdecl void glValidateProgramPipeline (unsigned-int))
  ;; void glGetProgramPipelineInfoLog(GLuint pipeline, GLsizei bufSize, GLsizei *length, GLchar *infoLog)
  (define-cdecl void glGetProgramPipelineInfoLog (unsigned-int int void* void*))
  ;; void glVertexAttribL1d(GLuint index, GLdouble x)
  (define-cdecl void glVertexAttribL1d (unsigned-int double))
  ;; void glVertexAttribL2d(GLuint index, GLdouble x, GLdouble y)
  (define-cdecl void glVertexAttribL2d (unsigned-int double double))
  ;; void glVertexAttribL3d(GLuint index, GLdouble x, GLdouble y, GLdouble z)
  (define-cdecl void glVertexAttribL3d (unsigned-int double double double))
  ;; void glVertexAttribL4d(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
  (define-cdecl void glVertexAttribL4d (unsigned-int double double double double))
  ;; void glVertexAttribL1dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttribL1dv (unsigned-int void*))
  ;; void glVertexAttribL2dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttribL2dv (unsigned-int void*))
  ;; void glVertexAttribL3dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttribL3dv (unsigned-int void*))
  ;; void glVertexAttribL4dv(GLuint index, const GLdouble *v)
  (define-cdecl void glVertexAttribL4dv (unsigned-int void*))
  ;; void glVertexAttribLPointer(GLuint index, GLint size, GLenum type, GLsizei stride, const void *pointer)
  (define-cdecl void glVertexAttribLPointer (unsigned-int int unsigned-int int void*))
  ;; void glGetVertexAttribLdv(GLuint index, GLenum pname, GLdouble *params)
  (define-cdecl void glGetVertexAttribLdv (unsigned-int unsigned-int void*))
  ;; void glViewportArrayv(GLuint first, GLsizei count, const GLfloat *v)
  (define-cdecl void glViewportArrayv (unsigned-int int void*))
  ;; void glViewportIndexedf(GLuint index, GLfloat x, GLfloat y, GLfloat w, GLfloat h)
  (define-cdecl void glViewportIndexedf (unsigned-int float float float float))
  ;; void glViewportIndexedfv(GLuint index, const GLfloat *v)
  (define-cdecl void glViewportIndexedfv (unsigned-int void*))
  ;; void glScissorArrayv(GLuint first, GLsizei count, const GLint *v)
  (define-cdecl void glScissorArrayv (unsigned-int int void*))
  ;; void glScissorIndexed(GLuint index, GLint left, GLint bottom, GLsizei width, GLsizei height)
  (define-cdecl void glScissorIndexed (unsigned-int int int int int))
  ;; void glScissorIndexedv(GLuint index, const GLint *v)
  (define-cdecl void glScissorIndexedv (unsigned-int void*))
  ;; void glDepthRangeArrayv(GLuint first, GLsizei count, const GLdouble *v)
  (define-cdecl void glDepthRangeArrayv (unsigned-int int void*))
  ;; void glDepthRangeIndexed(GLuint index, GLdouble n, GLdouble f)
  (define-cdecl void glDepthRangeIndexed (unsigned-int double double))
  ;; void glDrawArraysInstancedBaseInstance(GLenum mode, GLint first, GLsizei count, GLsizei instancecount, GLuint baseinstance)
  (define-cdecl void glDrawArraysInstancedBaseInstance (unsigned-int int int int unsigned-int))
  ;; void glDrawElementsInstancedBaseInstance(GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei instancecount, GLuint baseinstance)
  (define-cdecl void glDrawElementsInstancedBaseInstance (unsigned-int int unsigned-int void* int unsigned-int))
  ;; void glDrawElementsInstancedBaseVertexBaseInstance(GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei instancecount, GLint basevertex, GLuint baseinstance)
  (define-cdecl void glDrawElementsInstancedBaseVertexBaseInstance (unsigned-int int unsigned-int void* int int unsigned-int))
  ;; void glGetInternalformativ(GLenum target, GLenum internalformat, GLenum pname, GLsizei count, GLint *params)
  (define-cdecl void glGetInternalformativ (unsigned-int unsigned-int unsigned-int int void*))
  ;; void glGetActiveAtomicCounterBufferiv(GLuint program, GLuint bufferIndex, GLenum pname, GLint *params)
  (define-cdecl void glGetActiveAtomicCounterBufferiv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glBindImageTexture(GLuint unit, GLuint texture, GLint level, GLboolean layered, GLint layer, GLenum access, GLenum format)
  (define-cdecl void glBindImageTexture (unsigned-int unsigned-int int uint8_t int unsigned-int unsigned-int))
  ;; void glMemoryBarrier(GLbitfield barriers)
  (define-cdecl void glMemoryBarrier (unsigned-int))
  ;; void glTexStorage1D(GLenum target, GLsizei levels, GLenum internalformat, GLsizei width)
  (define-cdecl void glTexStorage1D (unsigned-int int unsigned-int int))
  ;; void glTexStorage2D(GLenum target, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glTexStorage2D (unsigned-int int unsigned-int int int))
  ;; void glTexStorage3D(GLenum target, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth)
  (define-cdecl void glTexStorage3D (unsigned-int int unsigned-int int int int))
  ;; void glDrawTransformFeedbackInstanced(GLenum mode, GLuint id, GLsizei instancecount)
  (define-cdecl void glDrawTransformFeedbackInstanced (unsigned-int unsigned-int int))
  ;; void glDrawTransformFeedbackStreamInstanced(GLenum mode, GLuint id, GLuint stream, GLsizei instancecount)
  (define-cdecl void glDrawTransformFeedbackStreamInstanced (unsigned-int unsigned-int unsigned-int int))
  ;; void glClearBufferData(GLenum target, GLenum internalformat, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearBufferData (unsigned-int unsigned-int unsigned-int unsigned-int void*))
  ;; void glClearBufferSubData(GLenum target, GLenum internalformat, GLintptr offset, GLsizeiptr size, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearBufferSubData (unsigned-int unsigned-int int int unsigned-int unsigned-int void*))
  ;; void glDispatchCompute(GLuint num_groups_x, GLuint num_groups_y, GLuint num_groups_z)
  (define-cdecl void glDispatchCompute (unsigned-int unsigned-int unsigned-int))
  ;; void glDispatchComputeIndirect(GLintptr indirect)
  (define-cdecl void glDispatchComputeIndirect (int))
  ;; void glCopyImageSubData(GLuint srcName, GLenum srcTarget, GLint srcLevel, GLint srcX, GLint srcY, GLint srcZ, GLuint dstName, GLenum dstTarget, GLint dstLevel, GLint dstX, GLint dstY, GLint dstZ, GLsizei srcWidth, GLsizei srcHeight, GLsizei srcDepth)
  (define-cdecl void glCopyImageSubData (unsigned-int unsigned-int int int int int unsigned-int unsigned-int int int int int int int int))
  ;; void glFramebufferParameteri(GLenum target, GLenum pname, GLint param)
  (define-cdecl void glFramebufferParameteri (unsigned-int unsigned-int int))
  ;; void glGetFramebufferParameteriv(GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetFramebufferParameteriv (unsigned-int unsigned-int void*))
  ;; void glGetInternalformati64v(GLenum target, GLenum internalformat, GLenum pname, GLsizei count, GLint64 *params)
  (define-cdecl void glGetInternalformati64v (unsigned-int unsigned-int unsigned-int int void*))
  ;; void glInvalidateTexSubImage(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth)
  (define-cdecl void glInvalidateTexSubImage (unsigned-int int int int int int int int))
  ;; void glInvalidateTexImage(GLuint texture, GLint level)
  (define-cdecl void glInvalidateTexImage (unsigned-int int))
  ;; void glInvalidateBufferSubData(GLuint buffer, GLintptr offset, GLsizeiptr length)
  (define-cdecl void glInvalidateBufferSubData (unsigned-int int int))
  ;; void glInvalidateBufferData(GLuint buffer)
  (define-cdecl void glInvalidateBufferData (unsigned-int))
  ;; void glInvalidateFramebuffer(GLenum target, GLsizei numAttachments, const GLenum *attachments)
  (define-cdecl void glInvalidateFramebuffer (unsigned-int int void*))
  ;; void glInvalidateSubFramebuffer(GLenum target, GLsizei numAttachments, const GLenum *attachments, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glInvalidateSubFramebuffer (unsigned-int int void* int int int int))
  ;; void glMultiDrawArraysIndirect(GLenum mode, const void *indirect, GLsizei drawcount, GLsizei stride)
  (define-cdecl void glMultiDrawArraysIndirect (unsigned-int void* int int))
  ;; void glMultiDrawElementsIndirect(GLenum mode, GLenum type, const void *indirect, GLsizei drawcount, GLsizei stride)
  (define-cdecl void glMultiDrawElementsIndirect (unsigned-int unsigned-int void* int int))
  ;; void glGetProgramInterfaceiv(GLuint program, GLenum programInterface, GLenum pname, GLint *params)
  (define-cdecl void glGetProgramInterfaceiv (unsigned-int unsigned-int unsigned-int void*))
  ;; GLuint glGetProgramResourceIndex(GLuint program, GLenum programInterface, const GLchar *name)
  (define-cdecl unsigned-int glGetProgramResourceIndex (unsigned-int unsigned-int void*))
  ;; void glGetProgramResourceName(GLuint program, GLenum programInterface, GLuint index, GLsizei bufSize, GLsizei *length, GLchar *name)
  (define-cdecl void glGetProgramResourceName (unsigned-int unsigned-int unsigned-int int void* void*))
  ;; void glGetProgramResourceiv(GLuint program, GLenum programInterface, GLuint index, GLsizei propCount, const GLenum *props, GLsizei count, GLsizei *length, GLint *params)
  (define-cdecl void glGetProgramResourceiv (unsigned-int unsigned-int unsigned-int int void* int void* void*))
  ;; GLint glGetProgramResourceLocation(GLuint program, GLenum programInterface, const GLchar *name)
  (define-cdecl int glGetProgramResourceLocation (unsigned-int unsigned-int void*))
  ;; GLint glGetProgramResourceLocationIndex(GLuint program, GLenum programInterface, const GLchar *name)
  (define-cdecl int glGetProgramResourceLocationIndex (unsigned-int unsigned-int void*))
  ;; void glShaderStorageBlockBinding(GLuint program, GLuint storageBlockIndex, GLuint storageBlockBinding)
  (define-cdecl void glShaderStorageBlockBinding (unsigned-int unsigned-int unsigned-int))
  ;; void glTexBufferRange(GLenum target, GLenum internalformat, GLuint buffer, GLintptr offset, GLsizeiptr size)
  (define-cdecl void glTexBufferRange (unsigned-int unsigned-int unsigned-int int int))
  ;; void glTexStorage2DMultisample(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLboolean fixedsamplelocations)
  (define-cdecl void glTexStorage2DMultisample (unsigned-int int unsigned-int int int uint8_t))
  ;; void glTexStorage3DMultisample(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean fixedsamplelocations)
  (define-cdecl void glTexStorage3DMultisample (unsigned-int int unsigned-int int int int uint8_t))
  ;; void glTextureView(GLuint texture, GLenum target, GLuint origtexture, GLenum internalformat, GLuint minlevel, GLuint numlevels, GLuint minlayer, GLuint numlayers)
  (define-cdecl void glTextureView (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glBindVertexBuffer(GLuint bindingindex, GLuint buffer, GLintptr offset, GLsizei stride)
  (define-cdecl void glBindVertexBuffer (unsigned-int unsigned-int int int))
  ;; void glVertexAttribFormat(GLuint attribindex, GLint size, GLenum type, GLboolean normalized, GLuint relativeoffset)
  (define-cdecl void glVertexAttribFormat (unsigned-int int unsigned-int uint8_t unsigned-int))
  ;; void glVertexAttribIFormat(GLuint attribindex, GLint size, GLenum type, GLuint relativeoffset)
  (define-cdecl void glVertexAttribIFormat (unsigned-int int unsigned-int unsigned-int))
  ;; void glVertexAttribLFormat(GLuint attribindex, GLint size, GLenum type, GLuint relativeoffset)
  (define-cdecl void glVertexAttribLFormat (unsigned-int int unsigned-int unsigned-int))
  ;; void glVertexAttribBinding(GLuint attribindex, GLuint bindingindex)
  (define-cdecl void glVertexAttribBinding (unsigned-int unsigned-int))
  ;; void glVertexBindingDivisor(GLuint bindingindex, GLuint divisor)
  (define-cdecl void glVertexBindingDivisor (unsigned-int unsigned-int))
  ;; void glDebugMessageControl(GLenum source, GLenum type, GLenum severity, GLsizei count, const GLuint *ids, GLboolean enabled)
  (define-cdecl void glDebugMessageControl (unsigned-int unsigned-int unsigned-int int void* uint8_t))
  ;; void glDebugMessageInsert(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *buf)
  (define-cdecl void glDebugMessageInsert (unsigned-int unsigned-int unsigned-int unsigned-int int void*))
  ;; void glDebugMessageCallback(GLDEBUGPROC callback, const void *userParam)
  (define-cdecl void glDebugMessageCallback (void* void*))
  ;; GLuint glGetDebugMessageLog(GLuint count, GLsizei bufSize, GLenum *sources, GLenum *types, GLuint *ids, GLenum *severities, GLsizei *lengths, GLchar *messageLog)
  (define-cdecl unsigned-int glGetDebugMessageLog (unsigned-int int void* void* void* void* void* void*))
  ;; void glPushDebugGroup(GLenum source, GLuint id, GLsizei length, const GLchar *message)
  (define-cdecl void glPushDebugGroup (unsigned-int unsigned-int int void*))
  ;; void glPopDebugGroup(void)
  (define-cdecl void glPopDebugGroup ())
  ;; void glObjectLabel(GLenum identifier, GLuint name, GLsizei length, const GLchar *label)
  (define-cdecl void glObjectLabel (unsigned-int unsigned-int int void*))
  ;; void glGetObjectLabel(GLenum identifier, GLuint name, GLsizei bufSize, GLsizei *length, GLchar *label)
  (define-cdecl void glGetObjectLabel (unsigned-int unsigned-int int void* void*))
  ;; void glObjectPtrLabel(const void *ptr, GLsizei length, const GLchar *label)
  (define-cdecl void glObjectPtrLabel (void* int void*))
  ;; void glGetObjectPtrLabel(const void *ptr, GLsizei bufSize, GLsizei *length, GLchar *label)
  (define-cdecl void glGetObjectPtrLabel (void* int void* void*))
  ;; void glBufferStorage(GLenum target, GLsizeiptr size, const void *data, GLbitfield flags)
  (define-cdecl void glBufferStorage (unsigned-int int void* unsigned-int))
  ;; void glClearTexImage(GLuint texture, GLint level, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearTexImage (unsigned-int int unsigned-int unsigned-int void*))
  ;; void glClearTexSubImage(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearTexSubImage (unsigned-int int int int int int int int unsigned-int unsigned-int void*))
  ;; void glBindBuffersBase(GLenum target, GLuint first, GLsizei count, const GLuint *buffers)
  (define-cdecl void glBindBuffersBase (unsigned-int unsigned-int int void*))
  ;; void glBindBuffersRange(GLenum target, GLuint first, GLsizei count, const GLuint *buffers, const GLintptr *offsets, const GLsizeiptr *sizes)
  (define-cdecl void glBindBuffersRange (unsigned-int unsigned-int int void* void* void*))
  ;; void glBindTextures(GLuint first, GLsizei count, const GLuint *textures)
  (define-cdecl void glBindTextures (unsigned-int int void*))
  ;; void glBindSamplers(GLuint first, GLsizei count, const GLuint *samplers)
  (define-cdecl void glBindSamplers (unsigned-int int void*))
  ;; void glBindImageTextures(GLuint first, GLsizei count, const GLuint *textures)
  (define-cdecl void glBindImageTextures (unsigned-int int void*))
  ;; void glBindVertexBuffers(GLuint first, GLsizei count, const GLuint *buffers, const GLintptr *offsets, const GLsizei *strides)
  (define-cdecl void glBindVertexBuffers (unsigned-int int void* void* void*))
  ;; void glClipControl(GLenum origin, GLenum depth)
  (define-cdecl void glClipControl (unsigned-int unsigned-int))
  ;; void glCreateTransformFeedbacks(GLsizei n, GLuint *ids)
  (define-cdecl void glCreateTransformFeedbacks (int void*))
  ;; void glTransformFeedbackBufferBase(GLuint xfb, GLuint index, GLuint buffer)
  (define-cdecl void glTransformFeedbackBufferBase (unsigned-int unsigned-int unsigned-int))
  ;; void glTransformFeedbackBufferRange(GLuint xfb, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size)
  (define-cdecl void glTransformFeedbackBufferRange (unsigned-int unsigned-int unsigned-int int int))
  ;; void glGetTransformFeedbackiv(GLuint xfb, GLenum pname, GLint *param)
  (define-cdecl void glGetTransformFeedbackiv (unsigned-int unsigned-int void*))
  ;; void glCreateBuffers(GLsizei n, GLuint *buffers)
  (define-cdecl void glCreateBuffers (int void*))
  ;; void glNamedBufferStorage(GLuint buffer, GLsizeiptr size, const void *data, GLbitfield flags)
  (define-cdecl void glNamedBufferStorage (unsigned-int int void* unsigned-int))
  ;; void glNamedBufferData(GLuint buffer, GLsizeiptr size, const void *data, GLenum usage)
  (define-cdecl void glNamedBufferData (unsigned-int int void* unsigned-int))
  ;; void glNamedBufferSubData(GLuint buffer, GLintptr offset, GLsizeiptr size, const void *data)
  (define-cdecl void glNamedBufferSubData (unsigned-int int int void*))
  ;; void glCopyNamedBufferSubData(GLuint readBuffer, GLuint writeBuffer, GLintptr readOffset, GLintptr writeOffset, GLsizeiptr size)
  (define-cdecl void glCopyNamedBufferSubData (unsigned-int unsigned-int int int int))
  ;; void glClearNamedBufferData(GLuint buffer, GLenum internalformat, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearNamedBufferData (unsigned-int unsigned-int unsigned-int unsigned-int void*))
  ;; void glClearNamedBufferSubData(GLuint buffer, GLenum internalformat, GLintptr offset, GLsizeiptr size, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearNamedBufferSubData (unsigned-int unsigned-int int int unsigned-int unsigned-int void*))
  ;; GLboolean glUnmapNamedBuffer(GLuint buffer)
  (define-cdecl uint8_t glUnmapNamedBuffer (unsigned-int))
  ;; void glFlushMappedNamedBufferRange(GLuint buffer, GLintptr offset, GLsizeiptr length)
  (define-cdecl void glFlushMappedNamedBufferRange (unsigned-int int int))
  ;; void glGetNamedBufferParameteriv(GLuint buffer, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedBufferParameteriv (unsigned-int unsigned-int void*))
  ;; void glGetNamedBufferParameteri64v(GLuint buffer, GLenum pname, GLint64 *params)
  (define-cdecl void glGetNamedBufferParameteri64v (unsigned-int unsigned-int void*))
  ;; void glGetNamedBufferPointerv(GLuint buffer, GLenum pname, void **params)
  (define-cdecl void glGetNamedBufferPointerv (unsigned-int unsigned-int void*))
  ;; void glGetNamedBufferSubData(GLuint buffer, GLintptr offset, GLsizeiptr size, void *data)
  (define-cdecl void glGetNamedBufferSubData (unsigned-int int int void*))
  ;; void glCreateFramebuffers(GLsizei n, GLuint *framebuffers)
  (define-cdecl void glCreateFramebuffers (int void*))
  ;; void glNamedFramebufferRenderbuffer(GLuint framebuffer, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)
  (define-cdecl void glNamedFramebufferRenderbuffer (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glNamedFramebufferParameteri(GLuint framebuffer, GLenum pname, GLint param)
  (define-cdecl void glNamedFramebufferParameteri (unsigned-int unsigned-int int))
  ;; void glNamedFramebufferTexture(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level)
  (define-cdecl void glNamedFramebufferTexture (unsigned-int unsigned-int unsigned-int int))
  ;; void glNamedFramebufferTextureLayer(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level, GLint layer)
  (define-cdecl void glNamedFramebufferTextureLayer (unsigned-int unsigned-int unsigned-int int int))
  ;; void glNamedFramebufferDrawBuffer(GLuint framebuffer, GLenum buf)
  (define-cdecl void glNamedFramebufferDrawBuffer (unsigned-int unsigned-int))
  ;; void glNamedFramebufferDrawBuffers(GLuint framebuffer, GLsizei n, const GLenum *bufs)
  (define-cdecl void glNamedFramebufferDrawBuffers (unsigned-int int void*))
  ;; void glNamedFramebufferReadBuffer(GLuint framebuffer, GLenum src)
  (define-cdecl void glNamedFramebufferReadBuffer (unsigned-int unsigned-int))
  ;; void glInvalidateNamedFramebufferData(GLuint framebuffer, GLsizei numAttachments, const GLenum *attachments)
  (define-cdecl void glInvalidateNamedFramebufferData (unsigned-int int void*))
  ;; void glInvalidateNamedFramebufferSubData(GLuint framebuffer, GLsizei numAttachments, const GLenum *attachments, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glInvalidateNamedFramebufferSubData (unsigned-int int void* int int int int))
  ;; void glClearNamedFramebufferiv(GLuint framebuffer, GLenum buffer, GLint drawbuffer, const GLint *value)
  (define-cdecl void glClearNamedFramebufferiv (unsigned-int unsigned-int int void*))
  ;; void glClearNamedFramebufferuiv(GLuint framebuffer, GLenum buffer, GLint drawbuffer, const GLuint *value)
  (define-cdecl void glClearNamedFramebufferuiv (unsigned-int unsigned-int int void*))
  ;; void glClearNamedFramebufferfv(GLuint framebuffer, GLenum buffer, GLint drawbuffer, const GLfloat *value)
  (define-cdecl void glClearNamedFramebufferfv (unsigned-int unsigned-int int void*))
  ;; void glClearNamedFramebufferfi(GLuint framebuffer, GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil)
  (define-cdecl void glClearNamedFramebufferfi (unsigned-int unsigned-int int float int))
  ;; void glBlitNamedFramebuffer(GLuint readFramebuffer, GLuint drawFramebuffer, GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0, GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter)
  (define-cdecl void glBlitNamedFramebuffer (unsigned-int unsigned-int int int int int int int int int unsigned-int unsigned-int))
  ;; GLenum glCheckNamedFramebufferStatus(GLuint framebuffer, GLenum target)
  (define-cdecl unsigned-int glCheckNamedFramebufferStatus (unsigned-int unsigned-int))
  ;; void glGetNamedFramebufferParameteriv(GLuint framebuffer, GLenum pname, GLint *param)
  (define-cdecl void glGetNamedFramebufferParameteriv (unsigned-int unsigned-int void*))
  ;; void glGetNamedFramebufferAttachmentParameteriv(GLuint framebuffer, GLenum attachment, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedFramebufferAttachmentParameteriv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glCreateRenderbuffers(GLsizei n, GLuint *renderbuffers)
  (define-cdecl void glCreateRenderbuffers (int void*))
  ;; void glNamedRenderbufferStorage(GLuint renderbuffer, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glNamedRenderbufferStorage (unsigned-int unsigned-int int int))
  ;; void glNamedRenderbufferStorageMultisample(GLuint renderbuffer, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glNamedRenderbufferStorageMultisample (unsigned-int int unsigned-int int int))
  ;; void glGetNamedRenderbufferParameteriv(GLuint renderbuffer, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedRenderbufferParameteriv (unsigned-int unsigned-int void*))
  ;; void glCreateTextures(GLenum target, GLsizei n, GLuint *textures)
  (define-cdecl void glCreateTextures (unsigned-int int void*))
  ;; void glTextureBuffer(GLuint texture, GLenum internalformat, GLuint buffer)
  (define-cdecl void glTextureBuffer (unsigned-int unsigned-int unsigned-int))
  ;; void glTextureBufferRange(GLuint texture, GLenum internalformat, GLuint buffer, GLintptr offset, GLsizeiptr size)
  (define-cdecl void glTextureBufferRange (unsigned-int unsigned-int unsigned-int int int))
  ;; void glTextureStorage1D(GLuint texture, GLsizei levels, GLenum internalformat, GLsizei width)
  (define-cdecl void glTextureStorage1D (unsigned-int int unsigned-int int))
  ;; void glTextureStorage2D(GLuint texture, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glTextureStorage2D (unsigned-int int unsigned-int int int))
  ;; void glTextureStorage3D(GLuint texture, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth)
  (define-cdecl void glTextureStorage3D (unsigned-int int unsigned-int int int int))
  ;; void glTextureStorage2DMultisample(GLuint texture, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLboolean fixedsamplelocations)
  (define-cdecl void glTextureStorage2DMultisample (unsigned-int int unsigned-int int int uint8_t))
  ;; void glTextureStorage3DMultisample(GLuint texture, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean fixedsamplelocations)
  (define-cdecl void glTextureStorage3DMultisample (unsigned-int int unsigned-int int int int uint8_t))
  ;; void glTextureSubImage1D(GLuint texture, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureSubImage1D (unsigned-int int int int unsigned-int unsigned-int void*))
  ;; void glTextureSubImage2D(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureSubImage2D (unsigned-int int int int int int unsigned-int unsigned-int void*))
  ;; void glTextureSubImage3D(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureSubImage3D (unsigned-int int int int int int int int unsigned-int unsigned-int void*))
  ;; void glCompressedTextureSubImage1D(GLuint texture, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTextureSubImage1D (unsigned-int int int int unsigned-int int void*))
  ;; void glCompressedTextureSubImage2D(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTextureSubImage2D (unsigned-int int int int int int unsigned-int int void*))
  ;; void glCompressedTextureSubImage3D(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const void *data)
  (define-cdecl void glCompressedTextureSubImage3D (unsigned-int int int int int int int int unsigned-int int void*))
  ;; void glCopyTextureSubImage1D(GLuint texture, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)
  (define-cdecl void glCopyTextureSubImage1D (unsigned-int int int int int int))
  ;; void glCopyTextureSubImage2D(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyTextureSubImage2D (unsigned-int int int int int int int int))
  ;; void glCopyTextureSubImage3D(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyTextureSubImage3D (unsigned-int int int int int int int int int))
  ;; void glTextureParameterf(GLuint texture, GLenum pname, GLfloat param)
  (define-cdecl void glTextureParameterf (unsigned-int unsigned-int float))
  ;; void glTextureParameterfv(GLuint texture, GLenum pname, const GLfloat *param)
  (define-cdecl void glTextureParameterfv (unsigned-int unsigned-int void*))
  ;; void glTextureParameteri(GLuint texture, GLenum pname, GLint param)
  (define-cdecl void glTextureParameteri (unsigned-int unsigned-int int))
  ;; void glTextureParameterIiv(GLuint texture, GLenum pname, const GLint *params)
  (define-cdecl void glTextureParameterIiv (unsigned-int unsigned-int void*))
  ;; void glTextureParameterIuiv(GLuint texture, GLenum pname, const GLuint *params)
  (define-cdecl void glTextureParameterIuiv (unsigned-int unsigned-int void*))
  ;; void glTextureParameteriv(GLuint texture, GLenum pname, const GLint *param)
  (define-cdecl void glTextureParameteriv (unsigned-int unsigned-int void*))
  ;; void glGenerateTextureMipmap(GLuint texture)
  (define-cdecl void glGenerateTextureMipmap (unsigned-int))
  ;; void glBindTextureUnit(GLuint unit, GLuint texture)
  (define-cdecl void glBindTextureUnit (unsigned-int unsigned-int))
  ;; void glGetTextureImage(GLuint texture, GLint level, GLenum format, GLenum type, GLsizei bufSize, void *pixels)
  (define-cdecl void glGetTextureImage (unsigned-int int unsigned-int unsigned-int int void*))
  ;; void glGetCompressedTextureImage(GLuint texture, GLint level, GLsizei bufSize, void *pixels)
  (define-cdecl void glGetCompressedTextureImage (unsigned-int int int void*))
  ;; void glGetTextureLevelParameterfv(GLuint texture, GLint level, GLenum pname, GLfloat *params)
  (define-cdecl void glGetTextureLevelParameterfv (unsigned-int int unsigned-int void*))
  ;; void glGetTextureLevelParameteriv(GLuint texture, GLint level, GLenum pname, GLint *params)
  (define-cdecl void glGetTextureLevelParameteriv (unsigned-int int unsigned-int void*))
  ;; void glGetTextureParameterfv(GLuint texture, GLenum pname, GLfloat *params)
  (define-cdecl void glGetTextureParameterfv (unsigned-int unsigned-int void*))
  ;; void glGetTextureParameterIiv(GLuint texture, GLenum pname, GLint *params)
  (define-cdecl void glGetTextureParameterIiv (unsigned-int unsigned-int void*))
  ;; void glGetTextureParameterIuiv(GLuint texture, GLenum pname, GLuint *params)
  (define-cdecl void glGetTextureParameterIuiv (unsigned-int unsigned-int void*))
  ;; void glGetTextureParameteriv(GLuint texture, GLenum pname, GLint *params)
  (define-cdecl void glGetTextureParameteriv (unsigned-int unsigned-int void*))
  ;; void glCreateVertexArrays(GLsizei n, GLuint *arrays)
  (define-cdecl void glCreateVertexArrays (int void*))
  ;; void glDisableVertexArrayAttrib(GLuint vaobj, GLuint index)
  (define-cdecl void glDisableVertexArrayAttrib (unsigned-int unsigned-int))
  ;; void glEnableVertexArrayAttrib(GLuint vaobj, GLuint index)
  (define-cdecl void glEnableVertexArrayAttrib (unsigned-int unsigned-int))
  ;; void glVertexArrayElementBuffer(GLuint vaobj, GLuint buffer)
  (define-cdecl void glVertexArrayElementBuffer (unsigned-int unsigned-int))
  ;; void glVertexArrayVertexBuffer(GLuint vaobj, GLuint bindingindex, GLuint buffer, GLintptr offset, GLsizei stride)
  (define-cdecl void glVertexArrayVertexBuffer (unsigned-int unsigned-int unsigned-int int int))
  ;; void glVertexArrayVertexBuffers(GLuint vaobj, GLuint first, GLsizei count, const GLuint *buffers, const GLintptr *offsets, const GLsizei *strides)
  (define-cdecl void glVertexArrayVertexBuffers (unsigned-int unsigned-int int void* void* void*))
  ;; void glVertexArrayAttribBinding(GLuint vaobj, GLuint attribindex, GLuint bindingindex)
  (define-cdecl void glVertexArrayAttribBinding (unsigned-int unsigned-int unsigned-int))
  ;; void glVertexArrayAttribFormat(GLuint vaobj, GLuint attribindex, GLint size, GLenum type, GLboolean normalized, GLuint relativeoffset)
  (define-cdecl void glVertexArrayAttribFormat (unsigned-int unsigned-int int unsigned-int uint8_t unsigned-int))
  ;; void glVertexArrayAttribIFormat(GLuint vaobj, GLuint attribindex, GLint size, GLenum type, GLuint relativeoffset)
  (define-cdecl void glVertexArrayAttribIFormat (unsigned-int unsigned-int int unsigned-int unsigned-int))
  ;; void glVertexArrayAttribLFormat(GLuint vaobj, GLuint attribindex, GLint size, GLenum type, GLuint relativeoffset)
  (define-cdecl void glVertexArrayAttribLFormat (unsigned-int unsigned-int int unsigned-int unsigned-int))
  ;; void glVertexArrayBindingDivisor(GLuint vaobj, GLuint bindingindex, GLuint divisor)
  (define-cdecl void glVertexArrayBindingDivisor (unsigned-int unsigned-int unsigned-int))
  ;; void glGetVertexArrayiv(GLuint vaobj, GLenum pname, GLint *param)
  (define-cdecl void glGetVertexArrayiv (unsigned-int unsigned-int void*))
  ;; void glGetVertexArrayIndexediv(GLuint vaobj, GLuint index, GLenum pname, GLint *param)
  (define-cdecl void glGetVertexArrayIndexediv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetVertexArrayIndexed64iv(GLuint vaobj, GLuint index, GLenum pname, GLint64 *param)
  (define-cdecl void glGetVertexArrayIndexed64iv (unsigned-int unsigned-int unsigned-int void*))
  ;; void glCreateSamplers(GLsizei n, GLuint *samplers)
  (define-cdecl void glCreateSamplers (int void*))
  ;; void glCreateProgramPipelines(GLsizei n, GLuint *pipelines)
  (define-cdecl void glCreateProgramPipelines (int void*))
  ;; void glCreateQueries(GLenum target, GLsizei n, GLuint *ids)
  (define-cdecl void glCreateQueries (unsigned-int int void*))
  ;; void glGetQueryBufferObjecti64v(GLuint id, GLuint buffer, GLenum pname, GLintptr offset)
  (define-cdecl void glGetQueryBufferObjecti64v (unsigned-int unsigned-int unsigned-int int))
  ;; void glGetQueryBufferObjectiv(GLuint id, GLuint buffer, GLenum pname, GLintptr offset)
  (define-cdecl void glGetQueryBufferObjectiv (unsigned-int unsigned-int unsigned-int int))
  ;; void glGetQueryBufferObjectui64v(GLuint id, GLuint buffer, GLenum pname, GLintptr offset)
  (define-cdecl void glGetQueryBufferObjectui64v (unsigned-int unsigned-int unsigned-int int))
  ;; void glGetQueryBufferObjectuiv(GLuint id, GLuint buffer, GLenum pname, GLintptr offset)
  (define-cdecl void glGetQueryBufferObjectuiv (unsigned-int unsigned-int unsigned-int int))
  ;; void glMemoryBarrierByRegion(GLbitfield barriers)
  (define-cdecl void glMemoryBarrierByRegion (unsigned-int))
  ;; void glGetTextureSubImage(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLsizei bufSize, void *pixels)
  (define-cdecl void glGetTextureSubImage (unsigned-int int int int int int int int unsigned-int unsigned-int int void*))
  ;; void glGetCompressedTextureSubImage(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLsizei bufSize, void *pixels)
  (define-cdecl void glGetCompressedTextureSubImage (unsigned-int int int int int int int int int void*))
  ;; GLenum glGetGraphicsResetStatus(void)
  (define-cdecl unsigned-int glGetGraphicsResetStatus ())
  ;; void glGetnCompressedTexImage(GLenum target, GLint lod, GLsizei bufSize, void *pixels)
  (define-cdecl void glGetnCompressedTexImage (unsigned-int int int void*))
  ;; void glGetnTexImage(GLenum target, GLint level, GLenum format, GLenum type, GLsizei bufSize, void *pixels)
  (define-cdecl void glGetnTexImage (unsigned-int int unsigned-int unsigned-int int void*))
  ;; void glGetnUniformdv(GLuint program, GLint location, GLsizei bufSize, GLdouble *params)
  (define-cdecl void glGetnUniformdv (unsigned-int int int void*))
  ;; void glGetnUniformfv(GLuint program, GLint location, GLsizei bufSize, GLfloat *params)
  (define-cdecl void glGetnUniformfv (unsigned-int int int void*))
  ;; void glGetnUniformiv(GLuint program, GLint location, GLsizei bufSize, GLint *params)
  (define-cdecl void glGetnUniformiv (unsigned-int int int void*))
  ;; void glGetnUniformuiv(GLuint program, GLint location, GLsizei bufSize, GLuint *params)
  (define-cdecl void glGetnUniformuiv (unsigned-int int int void*))
  ;; void glReadnPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLsizei bufSize, void *data)
  (define-cdecl void glReadnPixels (int int int int unsigned-int unsigned-int int void*))
  ;; void glTextureBarrier(void)
  (define-cdecl void glTextureBarrier ())
  ;; void glSpecializeShader(GLuint shader, const GLchar *pEntryPoint, GLuint numSpecializationConstants, const GLuint *pConstantIndex, const GLuint *pConstantValue)
  (define-cdecl void glSpecializeShader (unsigned-int void* unsigned-int void* void*))
  ;; void glMultiDrawArraysIndirectCount(GLenum mode, const void *indirect, GLintptr drawcount, GLsizei maxdrawcount, GLsizei stride)
  (define-cdecl void glMultiDrawArraysIndirectCount (unsigned-int void* int int int))
  ;; void glMultiDrawElementsIndirectCount(GLenum mode, GLenum type, const void *indirect, GLintptr drawcount, GLsizei maxdrawcount, GLsizei stride)
  (define-cdecl void glMultiDrawElementsIndirectCount (unsigned-int unsigned-int void* int int int))
  ;; void glPolygonOffsetClamp(GLfloat factor, GLfloat units, GLfloat clamp)
  (define-cdecl void glPolygonOffsetClamp (float float float))
  ;; void glPrimitiveBoundingBoxARB(GLfloat minX, GLfloat minY, GLfloat minZ, GLfloat minW, GLfloat maxX, GLfloat maxY, GLfloat maxZ, GLfloat maxW)
  (define-cdecl void glPrimitiveBoundingBoxARB (float float float float float float float float))
  ;; GLuint64 glGetTextureHandleARB(GLuint texture)
  (define-cdecl unsigned-int glGetTextureHandleARB (unsigned-int))
  ;; GLuint64 glGetTextureSamplerHandleARB(GLuint texture, GLuint sampler)
  (define-cdecl unsigned-int glGetTextureSamplerHandleARB (unsigned-int unsigned-int))
  ;; void glMakeTextureHandleResidentARB(GLuint64 handle)
  (define-cdecl void glMakeTextureHandleResidentARB (unsigned-int))
  ;; void glMakeTextureHandleNonResidentARB(GLuint64 handle)
  (define-cdecl void glMakeTextureHandleNonResidentARB (unsigned-int))
  ;; GLuint64 glGetImageHandleARB(GLuint texture, GLint level, GLboolean layered, GLint layer, GLenum format)
  (define-cdecl unsigned-int glGetImageHandleARB (unsigned-int int uint8_t int unsigned-int))
  ;; void glMakeImageHandleResidentARB(GLuint64 handle, GLenum access)
  (define-cdecl void glMakeImageHandleResidentARB (unsigned-int unsigned-int))
  ;; void glMakeImageHandleNonResidentARB(GLuint64 handle)
  (define-cdecl void glMakeImageHandleNonResidentARB (unsigned-int))
  ;; void glUniformHandleui64ARB(GLint location, GLuint64 value)
  (define-cdecl void glUniformHandleui64ARB (int unsigned-int))
  ;; void glUniformHandleui64vARB(GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glUniformHandleui64vARB (int int void*))
  ;; void glProgramUniformHandleui64ARB(GLuint program, GLint location, GLuint64 value)
  (define-cdecl void glProgramUniformHandleui64ARB (unsigned-int int unsigned-int))
  ;; void glProgramUniformHandleui64vARB(GLuint program, GLint location, GLsizei count, const GLuint64 *values)
  (define-cdecl void glProgramUniformHandleui64vARB (unsigned-int int int void*))
  ;; GLboolean glIsTextureHandleResidentARB(GLuint64 handle)
  (define-cdecl uint8_t glIsTextureHandleResidentARB (unsigned-int))
  ;; GLboolean glIsImageHandleResidentARB(GLuint64 handle)
  (define-cdecl uint8_t glIsImageHandleResidentARB (unsigned-int))
  ;; void glVertexAttribL1ui64ARB(GLuint index, GLuint64EXT x)
  (define-cdecl void glVertexAttribL1ui64ARB (unsigned-int unsigned-int))
  ;; void glVertexAttribL1ui64vARB(GLuint index, const GLuint64EXT *v)
  (define-cdecl void glVertexAttribL1ui64vARB (unsigned-int void*))
  ;; void glGetVertexAttribLui64vARB(GLuint index, GLenum pname, GLuint64EXT *params)
  (define-cdecl void glGetVertexAttribLui64vARB (unsigned-int unsigned-int void*))
  ;; GLsync glCreateSyncFromCLeventARB(struct _cl_context *context, struct _cl_event *event, GLbitfield flags)
  (define-cdecl void* glCreateSyncFromCLeventARB (void* void* unsigned-int))
  ;; void glDispatchComputeGroupSizeARB(GLuint num_groups_x, GLuint num_groups_y, GLuint num_groups_z, GLuint group_size_x, GLuint group_size_y, GLuint group_size_z)
  (define-cdecl void glDispatchComputeGroupSizeARB (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glDebugMessageControlARB(GLenum source, GLenum type, GLenum severity, GLsizei count, const GLuint *ids, GLboolean enabled)
  (define-cdecl void glDebugMessageControlARB (unsigned-int unsigned-int unsigned-int int void* uint8_t))
  ;; void glDebugMessageInsertARB(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *buf)
  (define-cdecl void glDebugMessageInsertARB (unsigned-int unsigned-int unsigned-int unsigned-int int void*))
  ;; void glDebugMessageCallbackARB(GLDEBUGPROCARB callback, const void *userParam)
  (define-cdecl void glDebugMessageCallbackARB (void* void*))
  ;; GLuint glGetDebugMessageLogARB(GLuint count, GLsizei bufSize, GLenum *sources, GLenum *types, GLuint *ids, GLenum *severities, GLsizei *lengths, GLchar *messageLog)
  (define-cdecl unsigned-int glGetDebugMessageLogARB (unsigned-int int void* void* void* void* void* void*))
  ;; void glBlendEquationiARB(GLuint buf, GLenum mode)
  (define-cdecl void glBlendEquationiARB (unsigned-int unsigned-int))
  ;; void glBlendEquationSeparateiARB(GLuint buf, GLenum modeRGB, GLenum modeAlpha)
  (define-cdecl void glBlendEquationSeparateiARB (unsigned-int unsigned-int unsigned-int))
  ;; void glBlendFunciARB(GLuint buf, GLenum src, GLenum dst)
  (define-cdecl void glBlendFunciARB (unsigned-int unsigned-int unsigned-int))
  ;; void glBlendFuncSeparateiARB(GLuint buf, GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha)
  (define-cdecl void glBlendFuncSeparateiARB (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glDrawArraysInstancedARB(GLenum mode, GLint first, GLsizei count, GLsizei primcount)
  (define-cdecl void glDrawArraysInstancedARB (unsigned-int int int int))
  ;; void glDrawElementsInstancedARB(GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei primcount)
  (define-cdecl void glDrawElementsInstancedARB (unsigned-int int unsigned-int void* int))
  ;; void glProgramParameteriARB(GLuint program, GLenum pname, GLint value)
  (define-cdecl void glProgramParameteriARB (unsigned-int unsigned-int int))
  ;; void glFramebufferTextureARB(GLenum target, GLenum attachment, GLuint texture, GLint level)
  (define-cdecl void glFramebufferTextureARB (unsigned-int unsigned-int unsigned-int int))
  ;; void glFramebufferTextureLayerARB(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer)
  (define-cdecl void glFramebufferTextureLayerARB (unsigned-int unsigned-int unsigned-int int int))
  ;; void glFramebufferTextureFaceARB(GLenum target, GLenum attachment, GLuint texture, GLint level, GLenum face)
  (define-cdecl void glFramebufferTextureFaceARB (unsigned-int unsigned-int unsigned-int int unsigned-int))
  ;; void glSpecializeShaderARB(GLuint shader, const GLchar *pEntryPoint, GLuint numSpecializationConstants, const GLuint *pConstantIndex, const GLuint *pConstantValue)
  (define-cdecl void glSpecializeShaderARB (unsigned-int void* unsigned-int void* void*))
  ;; void glUniform1i64ARB(GLint location, GLint64 x)
  (define-cdecl void glUniform1i64ARB (int int))
  ;; void glUniform2i64ARB(GLint location, GLint64 x, GLint64 y)
  (define-cdecl void glUniform2i64ARB (int int int))
  ;; void glUniform3i64ARB(GLint location, GLint64 x, GLint64 y, GLint64 z)
  (define-cdecl void glUniform3i64ARB (int int int int))
  ;; void glUniform4i64ARB(GLint location, GLint64 x, GLint64 y, GLint64 z, GLint64 w)
  (define-cdecl void glUniform4i64ARB (int int int int int))
  ;; void glUniform1i64vARB(GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glUniform1i64vARB (int int void*))
  ;; void glUniform2i64vARB(GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glUniform2i64vARB (int int void*))
  ;; void glUniform3i64vARB(GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glUniform3i64vARB (int int void*))
  ;; void glUniform4i64vARB(GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glUniform4i64vARB (int int void*))
  ;; void glUniform1ui64ARB(GLint location, GLuint64 x)
  (define-cdecl void glUniform1ui64ARB (int unsigned-int))
  ;; void glUniform2ui64ARB(GLint location, GLuint64 x, GLuint64 y)
  (define-cdecl void glUniform2ui64ARB (int unsigned-int unsigned-int))
  ;; void glUniform3ui64ARB(GLint location, GLuint64 x, GLuint64 y, GLuint64 z)
  (define-cdecl void glUniform3ui64ARB (int unsigned-int unsigned-int unsigned-int))
  ;; void glUniform4ui64ARB(GLint location, GLuint64 x, GLuint64 y, GLuint64 z, GLuint64 w)
  (define-cdecl void glUniform4ui64ARB (int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glUniform1ui64vARB(GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glUniform1ui64vARB (int int void*))
  ;; void glUniform2ui64vARB(GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glUniform2ui64vARB (int int void*))
  ;; void glUniform3ui64vARB(GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glUniform3ui64vARB (int int void*))
  ;; void glUniform4ui64vARB(GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glUniform4ui64vARB (int int void*))
  ;; void glGetUniformi64vARB(GLuint program, GLint location, GLint64 *params)
  (define-cdecl void glGetUniformi64vARB (unsigned-int int void*))
  ;; void glGetUniformui64vARB(GLuint program, GLint location, GLuint64 *params)
  (define-cdecl void glGetUniformui64vARB (unsigned-int int void*))
  ;; void glGetnUniformi64vARB(GLuint program, GLint location, GLsizei bufSize, GLint64 *params)
  (define-cdecl void glGetnUniformi64vARB (unsigned-int int int void*))
  ;; void glGetnUniformui64vARB(GLuint program, GLint location, GLsizei bufSize, GLuint64 *params)
  (define-cdecl void glGetnUniformui64vARB (unsigned-int int int void*))
  ;; void glProgramUniform1i64ARB(GLuint program, GLint location, GLint64 x)
  (define-cdecl void glProgramUniform1i64ARB (unsigned-int int int))
  ;; void glProgramUniform2i64ARB(GLuint program, GLint location, GLint64 x, GLint64 y)
  (define-cdecl void glProgramUniform2i64ARB (unsigned-int int int int))
  ;; void glProgramUniform3i64ARB(GLuint program, GLint location, GLint64 x, GLint64 y, GLint64 z)
  (define-cdecl void glProgramUniform3i64ARB (unsigned-int int int int int))
  ;; void glProgramUniform4i64ARB(GLuint program, GLint location, GLint64 x, GLint64 y, GLint64 z, GLint64 w)
  (define-cdecl void glProgramUniform4i64ARB (unsigned-int int int int int int))
  ;; void glProgramUniform1i64vARB(GLuint program, GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glProgramUniform1i64vARB (unsigned-int int int void*))
  ;; void glProgramUniform2i64vARB(GLuint program, GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glProgramUniform2i64vARB (unsigned-int int int void*))
  ;; void glProgramUniform3i64vARB(GLuint program, GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glProgramUniform3i64vARB (unsigned-int int int void*))
  ;; void glProgramUniform4i64vARB(GLuint program, GLint location, GLsizei count, const GLint64 *value)
  (define-cdecl void glProgramUniform4i64vARB (unsigned-int int int void*))
  ;; void glProgramUniform1ui64ARB(GLuint program, GLint location, GLuint64 x)
  (define-cdecl void glProgramUniform1ui64ARB (unsigned-int int unsigned-int))
  ;; void glProgramUniform2ui64ARB(GLuint program, GLint location, GLuint64 x, GLuint64 y)
  (define-cdecl void glProgramUniform2ui64ARB (unsigned-int int unsigned-int unsigned-int))
  ;; void glProgramUniform3ui64ARB(GLuint program, GLint location, GLuint64 x, GLuint64 y, GLuint64 z)
  (define-cdecl void glProgramUniform3ui64ARB (unsigned-int int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform4ui64ARB(GLuint program, GLint location, GLuint64 x, GLuint64 y, GLuint64 z, GLuint64 w)
  (define-cdecl void glProgramUniform4ui64ARB (unsigned-int int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform1ui64vARB(GLuint program, GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glProgramUniform1ui64vARB (unsigned-int int int void*))
  ;; void glProgramUniform2ui64vARB(GLuint program, GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glProgramUniform2ui64vARB (unsigned-int int int void*))
  ;; void glProgramUniform3ui64vARB(GLuint program, GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glProgramUniform3ui64vARB (unsigned-int int int void*))
  ;; void glProgramUniform4ui64vARB(GLuint program, GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glProgramUniform4ui64vARB (unsigned-int int int void*))
  ;; void glMultiDrawArraysIndirectCountARB(GLenum mode, const void *indirect, GLintptr drawcount, GLsizei maxdrawcount, GLsizei stride)
  (define-cdecl void glMultiDrawArraysIndirectCountARB (unsigned-int void* int int int))
  ;; void glMultiDrawElementsIndirectCountARB(GLenum mode, GLenum type, const void *indirect, GLintptr drawcount, GLsizei maxdrawcount, GLsizei stride)
  (define-cdecl void glMultiDrawElementsIndirectCountARB (unsigned-int unsigned-int void* int int int))
  ;; void glVertexAttribDivisorARB(GLuint index, GLuint divisor)
  (define-cdecl void glVertexAttribDivisorARB (unsigned-int unsigned-int))
  ;; void glMaxShaderCompilerThreadsARB(GLuint count)
  (define-cdecl void glMaxShaderCompilerThreadsARB (unsigned-int))
  ;; GLenum glGetGraphicsResetStatusARB(void)
  (define-cdecl unsigned-int glGetGraphicsResetStatusARB ())
  ;; void glGetnTexImageARB(GLenum target, GLint level, GLenum format, GLenum type, GLsizei bufSize, void *img)
  (define-cdecl void glGetnTexImageARB (unsigned-int int unsigned-int unsigned-int int void*))
  ;; void glReadnPixelsARB(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLsizei bufSize, void *data)
  (define-cdecl void glReadnPixelsARB (int int int int unsigned-int unsigned-int int void*))
  ;; void glGetnCompressedTexImageARB(GLenum target, GLint lod, GLsizei bufSize, void *img)
  (define-cdecl void glGetnCompressedTexImageARB (unsigned-int int int void*))
  ;; void glGetnUniformfvARB(GLuint program, GLint location, GLsizei bufSize, GLfloat *params)
  (define-cdecl void glGetnUniformfvARB (unsigned-int int int void*))
  ;; void glGetnUniformivARB(GLuint program, GLint location, GLsizei bufSize, GLint *params)
  (define-cdecl void glGetnUniformivARB (unsigned-int int int void*))
  ;; void glGetnUniformuivARB(GLuint program, GLint location, GLsizei bufSize, GLuint *params)
  (define-cdecl void glGetnUniformuivARB (unsigned-int int int void*))
  ;; void glGetnUniformdvARB(GLuint program, GLint location, GLsizei bufSize, GLdouble *params)
  (define-cdecl void glGetnUniformdvARB (unsigned-int int int void*))
  ;; void glFramebufferSampleLocationsfvARB(GLenum target, GLuint start, GLsizei count, const GLfloat *v)
  (define-cdecl void glFramebufferSampleLocationsfvARB (unsigned-int unsigned-int int void*))
  ;; void glNamedFramebufferSampleLocationsfvARB(GLuint framebuffer, GLuint start, GLsizei count, const GLfloat *v)
  (define-cdecl void glNamedFramebufferSampleLocationsfvARB (unsigned-int unsigned-int int void*))
  ;; void glEvaluateDepthValuesARB(void)
  (define-cdecl void glEvaluateDepthValuesARB ())
  ;; void glMinSampleShadingARB(GLfloat value)
  (define-cdecl void glMinSampleShadingARB (float))
  ;; void glNamedStringARB(GLenum type, GLint namelen, const GLchar *name, GLint stringlen, const GLchar *string)
  (define-cdecl void glNamedStringARB (unsigned-int int void* int void*))
  ;; void glDeleteNamedStringARB(GLint namelen, const GLchar *name)
  (define-cdecl void glDeleteNamedStringARB (int void*))
  ;; void glCompileShaderIncludeARB(GLuint shader, GLsizei count, const GLchar *const*path, const GLint *length)
  (define-cdecl void glCompileShaderIncludeARB (unsigned-int int void* void*))
  ;; GLboolean glIsNamedStringARB(GLint namelen, const GLchar *name)
  (define-cdecl uint8_t glIsNamedStringARB (int void*))
  ;; void glGetNamedStringARB(GLint namelen, const GLchar *name, GLsizei bufSize, GLint *stringlen, GLchar *string)
  (define-cdecl void glGetNamedStringARB (int void* int void* void*))
  ;; void glGetNamedStringivARB(GLint namelen, const GLchar *name, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedStringivARB (int void* unsigned-int void*))
  ;; void glBufferPageCommitmentARB(GLenum target, GLintptr offset, GLsizeiptr size, GLboolean commit)
  (define-cdecl void glBufferPageCommitmentARB (unsigned-int int int uint8_t))
  ;; void glNamedBufferPageCommitmentEXT(GLuint buffer, GLintptr offset, GLsizeiptr size, GLboolean commit)
  (define-cdecl void glNamedBufferPageCommitmentEXT (unsigned-int int int uint8_t))
  ;; void glNamedBufferPageCommitmentARB(GLuint buffer, GLintptr offset, GLsizeiptr size, GLboolean commit)
  (define-cdecl void glNamedBufferPageCommitmentARB (unsigned-int int int uint8_t))
  ;; void glTexPageCommitmentARB(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLboolean commit)
  (define-cdecl void glTexPageCommitmentARB (unsigned-int int int int int int int int uint8_t))
  ;; void glTexBufferARB(GLenum target, GLenum internalformat, GLuint buffer)
  (define-cdecl void glTexBufferARB (unsigned-int unsigned-int unsigned-int))
  ;; void glDepthRangeArraydvNV(GLuint first, GLsizei count, const GLdouble *v)
  (define-cdecl void glDepthRangeArraydvNV (unsigned-int int void*))
  ;; void glDepthRangeIndexeddNV(GLuint index, GLdouble n, GLdouble f)
  (define-cdecl void glDepthRangeIndexeddNV (unsigned-int double double))
  ;; void glBlendBarrierKHR(void)
  (define-cdecl void glBlendBarrierKHR ())
  ;; void glMaxShaderCompilerThreadsKHR(GLuint count)
  (define-cdecl void glMaxShaderCompilerThreadsKHR (unsigned-int))
  ;; void glRenderbufferStorageMultisampleAdvancedAMD(GLenum target, GLsizei samples, GLsizei storageSamples, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glRenderbufferStorageMultisampleAdvancedAMD (unsigned-int int int unsigned-int int int))
  ;; void glNamedRenderbufferStorageMultisampleAdvancedAMD(GLuint renderbuffer, GLsizei samples, GLsizei storageSamples, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glNamedRenderbufferStorageMultisampleAdvancedAMD (unsigned-int int int unsigned-int int int))
  ;; void glGetPerfMonitorGroupsAMD(GLint *numGroups, GLsizei groupsSize, GLuint *groups)
  (define-cdecl void glGetPerfMonitorGroupsAMD (void* int void*))
  ;; void glGetPerfMonitorCountersAMD(GLuint group, GLint *numCounters, GLint *maxActiveCounters, GLsizei counterSize, GLuint *counters)
  (define-cdecl void glGetPerfMonitorCountersAMD (unsigned-int void* void* int void*))
  ;; void glGetPerfMonitorGroupStringAMD(GLuint group, GLsizei bufSize, GLsizei *length, GLchar *groupString)
  (define-cdecl void glGetPerfMonitorGroupStringAMD (unsigned-int int void* void*))
  ;; void glGetPerfMonitorCounterStringAMD(GLuint group, GLuint counter, GLsizei bufSize, GLsizei *length, GLchar *counterString)
  (define-cdecl void glGetPerfMonitorCounterStringAMD (unsigned-int unsigned-int int void* void*))
  ;; void glGetPerfMonitorCounterInfoAMD(GLuint group, GLuint counter, GLenum pname, void *data)
  (define-cdecl void glGetPerfMonitorCounterInfoAMD (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGenPerfMonitorsAMD(GLsizei n, GLuint *monitors)
  (define-cdecl void glGenPerfMonitorsAMD (int void*))
  ;; void glDeletePerfMonitorsAMD(GLsizei n, GLuint *monitors)
  (define-cdecl void glDeletePerfMonitorsAMD (int void*))
  ;; void glSelectPerfMonitorCountersAMD(GLuint monitor, GLboolean enable, GLuint group, GLint numCounters, GLuint *counterList)
  (define-cdecl void glSelectPerfMonitorCountersAMD (unsigned-int uint8_t unsigned-int int void*))
  ;; void glBeginPerfMonitorAMD(GLuint monitor)
  (define-cdecl void glBeginPerfMonitorAMD (unsigned-int))
  ;; void glEndPerfMonitorAMD(GLuint monitor)
  (define-cdecl void glEndPerfMonitorAMD (unsigned-int))
  ;; void glGetPerfMonitorCounterDataAMD(GLuint monitor, GLenum pname, GLsizei dataSize, GLuint *data, GLint *bytesWritten)
  (define-cdecl void glGetPerfMonitorCounterDataAMD (unsigned-int unsigned-int int void* void*))
  ;; void glEGLImageTargetTexStorageEXT(GLenum target, GLeglImageOES image, const GLint* attrib_list)
  (define-cdecl void glEGLImageTargetTexStorageEXT (unsigned-int void* void*))
  ;; void glEGLImageTargetTextureStorageEXT(GLuint texture, GLeglImageOES image, const GLint* attrib_list)
  (define-cdecl void glEGLImageTargetTextureStorageEXT (unsigned-int void* void*))
  ;; void glLabelObjectEXT(GLenum type, GLuint object, GLsizei length, const GLchar *label)
  (define-cdecl void glLabelObjectEXT (unsigned-int unsigned-int int void*))
  ;; void glGetObjectLabelEXT(GLenum type, GLuint object, GLsizei bufSize, GLsizei *length, GLchar *label)
  (define-cdecl void glGetObjectLabelEXT (unsigned-int unsigned-int int void* void*))
  ;; void glInsertEventMarkerEXT(GLsizei length, const GLchar *marker)
  (define-cdecl void glInsertEventMarkerEXT (int void*))
  ;; void glPushGroupMarkerEXT(GLsizei length, const GLchar *marker)
  (define-cdecl void glPushGroupMarkerEXT (int void*))
  ;; void glPopGroupMarkerEXT(void)
  (define-cdecl void glPopGroupMarkerEXT ())
  ;; void glMatrixLoadfEXT(GLenum mode, const GLfloat *m)
  (define-cdecl void glMatrixLoadfEXT (unsigned-int void*))
  ;; void glMatrixLoaddEXT(GLenum mode, const GLdouble *m)
  (define-cdecl void glMatrixLoaddEXT (unsigned-int void*))
  ;; void glMatrixMultfEXT(GLenum mode, const GLfloat *m)
  (define-cdecl void glMatrixMultfEXT (unsigned-int void*))
  ;; void glMatrixMultdEXT(GLenum mode, const GLdouble *m)
  (define-cdecl void glMatrixMultdEXT (unsigned-int void*))
  ;; void glMatrixLoadIdentityEXT(GLenum mode)
  (define-cdecl void glMatrixLoadIdentityEXT (unsigned-int))
  ;; void glMatrixRotatefEXT(GLenum mode, GLfloat angle, GLfloat x, GLfloat y, GLfloat z)
  (define-cdecl void glMatrixRotatefEXT (unsigned-int float float float float))
  ;; void glMatrixRotatedEXT(GLenum mode, GLdouble angle, GLdouble x, GLdouble y, GLdouble z)
  (define-cdecl void glMatrixRotatedEXT (unsigned-int double double double double))
  ;; void glMatrixScalefEXT(GLenum mode, GLfloat x, GLfloat y, GLfloat z)
  (define-cdecl void glMatrixScalefEXT (unsigned-int float float float))
  ;; void glMatrixScaledEXT(GLenum mode, GLdouble x, GLdouble y, GLdouble z)
  (define-cdecl void glMatrixScaledEXT (unsigned-int double double double))
  ;; void glMatrixTranslatefEXT(GLenum mode, GLfloat x, GLfloat y, GLfloat z)
  (define-cdecl void glMatrixTranslatefEXT (unsigned-int float float float))
  ;; void glMatrixTranslatedEXT(GLenum mode, GLdouble x, GLdouble y, GLdouble z)
  (define-cdecl void glMatrixTranslatedEXT (unsigned-int double double double))
  ;; void glMatrixFrustumEXT(GLenum mode, GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
  (define-cdecl void glMatrixFrustumEXT (unsigned-int double double double double double double))
  ;; void glMatrixOrthoEXT(GLenum mode, GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
  (define-cdecl void glMatrixOrthoEXT (unsigned-int double double double double double double))
  ;; void glMatrixPopEXT(GLenum mode)
  (define-cdecl void glMatrixPopEXT (unsigned-int))
  ;; void glMatrixPushEXT(GLenum mode)
  (define-cdecl void glMatrixPushEXT (unsigned-int))
  ;; void glClientAttribDefaultEXT(GLbitfield mask)
  (define-cdecl void glClientAttribDefaultEXT (unsigned-int))
  ;; void glPushClientAttribDefaultEXT(GLbitfield mask)
  (define-cdecl void glPushClientAttribDefaultEXT (unsigned-int))
  ;; void glTextureParameterfEXT(GLuint texture, GLenum target, GLenum pname, GLfloat param)
  (define-cdecl void glTextureParameterfEXT (unsigned-int unsigned-int unsigned-int float))
  ;; void glTextureParameterfvEXT(GLuint texture, GLenum target, GLenum pname, const GLfloat *params)
  (define-cdecl void glTextureParameterfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glTextureParameteriEXT(GLuint texture, GLenum target, GLenum pname, GLint param)
  (define-cdecl void glTextureParameteriEXT (unsigned-int unsigned-int unsigned-int int))
  ;; void glTextureParameterivEXT(GLuint texture, GLenum target, GLenum pname, const GLint *params)
  (define-cdecl void glTextureParameterivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glTextureImage1DEXT(GLuint texture, GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureImage1DEXT (unsigned-int unsigned-int int int int int unsigned-int unsigned-int void*))
  ;; void glTextureImage2DEXT(GLuint texture, GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureImage2DEXT (unsigned-int unsigned-int int int int int int unsigned-int unsigned-int void*))
  ;; void glTextureSubImage1DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureSubImage1DEXT (unsigned-int unsigned-int int int int unsigned-int unsigned-int void*))
  ;; void glTextureSubImage2DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureSubImage2DEXT (unsigned-int unsigned-int int int int int int unsigned-int unsigned-int void*))
  ;; void glCopyTextureImage1DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)
  (define-cdecl void glCopyTextureImage1DEXT (unsigned-int unsigned-int int unsigned-int int int int int))
  ;; void glCopyTextureImage2DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)
  (define-cdecl void glCopyTextureImage2DEXT (unsigned-int unsigned-int int unsigned-int int int int int int))
  ;; void glCopyTextureSubImage1DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)
  (define-cdecl void glCopyTextureSubImage1DEXT (unsigned-int unsigned-int int int int int int))
  ;; void glCopyTextureSubImage2DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyTextureSubImage2DEXT (unsigned-int unsigned-int int int int int int int int))
  ;; void glGetTextureImageEXT(GLuint texture, GLenum target, GLint level, GLenum format, GLenum type, void *pixels)
  (define-cdecl void glGetTextureImageEXT (unsigned-int unsigned-int int unsigned-int unsigned-int void*))
  ;; void glGetTextureParameterfvEXT(GLuint texture, GLenum target, GLenum pname, GLfloat *params)
  (define-cdecl void glGetTextureParameterfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetTextureParameterivEXT(GLuint texture, GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetTextureParameterivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetTextureLevelParameterfvEXT(GLuint texture, GLenum target, GLint level, GLenum pname, GLfloat *params)
  (define-cdecl void glGetTextureLevelParameterfvEXT (unsigned-int unsigned-int int unsigned-int void*))
  ;; void glGetTextureLevelParameterivEXT(GLuint texture, GLenum target, GLint level, GLenum pname, GLint *params)
  (define-cdecl void glGetTextureLevelParameterivEXT (unsigned-int unsigned-int int unsigned-int void*))
  ;; void glTextureImage3DEXT(GLuint texture, GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureImage3DEXT (unsigned-int unsigned-int int int int int int int unsigned-int unsigned-int void*))
  ;; void glTextureSubImage3DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glTextureSubImage3DEXT (unsigned-int unsigned-int int int int int int int int unsigned-int unsigned-int void*))
  ;; void glCopyTextureSubImage3DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyTextureSubImage3DEXT (unsigned-int unsigned-int int int int int int int int int))
  ;; void glBindMultiTextureEXT(GLenum texunit, GLenum target, GLuint texture)
  (define-cdecl void glBindMultiTextureEXT (unsigned-int unsigned-int unsigned-int))
  ;; void glMultiTexCoordPointerEXT(GLenum texunit, GLint size, GLenum type, GLsizei stride, const void *pointer)
  (define-cdecl void glMultiTexCoordPointerEXT (unsigned-int int unsigned-int int void*))
  ;; void glMultiTexEnvfEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat param)
  (define-cdecl void glMultiTexEnvfEXT (unsigned-int unsigned-int unsigned-int float))
  ;; void glMultiTexEnvfvEXT(GLenum texunit, GLenum target, GLenum pname, const GLfloat *params)
  (define-cdecl void glMultiTexEnvfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexEnviEXT(GLenum texunit, GLenum target, GLenum pname, GLint param)
  (define-cdecl void glMultiTexEnviEXT (unsigned-int unsigned-int unsigned-int int))
  ;; void glMultiTexEnvivEXT(GLenum texunit, GLenum target, GLenum pname, const GLint *params)
  (define-cdecl void glMultiTexEnvivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexGendEXT(GLenum texunit, GLenum coord, GLenum pname, GLdouble param)
  (define-cdecl void glMultiTexGendEXT (unsigned-int unsigned-int unsigned-int double))
  ;; void glMultiTexGendvEXT(GLenum texunit, GLenum coord, GLenum pname, const GLdouble *params)
  (define-cdecl void glMultiTexGendvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexGenfEXT(GLenum texunit, GLenum coord, GLenum pname, GLfloat param)
  (define-cdecl void glMultiTexGenfEXT (unsigned-int unsigned-int unsigned-int float))
  ;; void glMultiTexGenfvEXT(GLenum texunit, GLenum coord, GLenum pname, const GLfloat *params)
  (define-cdecl void glMultiTexGenfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexGeniEXT(GLenum texunit, GLenum coord, GLenum pname, GLint param)
  (define-cdecl void glMultiTexGeniEXT (unsigned-int unsigned-int unsigned-int int))
  ;; void glMultiTexGenivEXT(GLenum texunit, GLenum coord, GLenum pname, const GLint *params)
  (define-cdecl void glMultiTexGenivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexEnvfvEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat *params)
  (define-cdecl void glGetMultiTexEnvfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexEnvivEXT(GLenum texunit, GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetMultiTexEnvivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexGendvEXT(GLenum texunit, GLenum coord, GLenum pname, GLdouble *params)
  (define-cdecl void glGetMultiTexGendvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexGenfvEXT(GLenum texunit, GLenum coord, GLenum pname, GLfloat *params)
  (define-cdecl void glGetMultiTexGenfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexGenivEXT(GLenum texunit, GLenum coord, GLenum pname, GLint *params)
  (define-cdecl void glGetMultiTexGenivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexParameteriEXT(GLenum texunit, GLenum target, GLenum pname, GLint param)
  (define-cdecl void glMultiTexParameteriEXT (unsigned-int unsigned-int unsigned-int int))
  ;; void glMultiTexParameterivEXT(GLenum texunit, GLenum target, GLenum pname, const GLint *params)
  (define-cdecl void glMultiTexParameterivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexParameterfEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat param)
  (define-cdecl void glMultiTexParameterfEXT (unsigned-int unsigned-int unsigned-int float))
  ;; void glMultiTexParameterfvEXT(GLenum texunit, GLenum target, GLenum pname, const GLfloat *params)
  (define-cdecl void glMultiTexParameterfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexImage1DEXT(GLenum texunit, GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glMultiTexImage1DEXT (unsigned-int unsigned-int int int int int unsigned-int unsigned-int void*))
  ;; void glMultiTexImage2DEXT(GLenum texunit, GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glMultiTexImage2DEXT (unsigned-int unsigned-int int int int int int unsigned-int unsigned-int void*))
  ;; void glMultiTexSubImage1DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glMultiTexSubImage1DEXT (unsigned-int unsigned-int int int int unsigned-int unsigned-int void*))
  ;; void glMultiTexSubImage2DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glMultiTexSubImage2DEXT (unsigned-int unsigned-int int int int int int unsigned-int unsigned-int void*))
  ;; void glCopyMultiTexImage1DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)
  (define-cdecl void glCopyMultiTexImage1DEXT (unsigned-int unsigned-int int unsigned-int int int int int))
  ;; void glCopyMultiTexImage2DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)
  (define-cdecl void glCopyMultiTexImage2DEXT (unsigned-int unsigned-int int unsigned-int int int int int int))
  ;; void glCopyMultiTexSubImage1DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)
  (define-cdecl void glCopyMultiTexSubImage1DEXT (unsigned-int unsigned-int int int int int int))
  ;; void glCopyMultiTexSubImage2DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyMultiTexSubImage2DEXT (unsigned-int unsigned-int int int int int int int int))
  ;; void glGetMultiTexImageEXT(GLenum texunit, GLenum target, GLint level, GLenum format, GLenum type, void *pixels)
  (define-cdecl void glGetMultiTexImageEXT (unsigned-int unsigned-int int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexParameterfvEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat *params)
  (define-cdecl void glGetMultiTexParameterfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexParameterivEXT(GLenum texunit, GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetMultiTexParameterivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexLevelParameterfvEXT(GLenum texunit, GLenum target, GLint level, GLenum pname, GLfloat *params)
  (define-cdecl void glGetMultiTexLevelParameterfvEXT (unsigned-int unsigned-int int unsigned-int void*))
  ;; void glGetMultiTexLevelParameterivEXT(GLenum texunit, GLenum target, GLint level, GLenum pname, GLint *params)
  (define-cdecl void glGetMultiTexLevelParameterivEXT (unsigned-int unsigned-int int unsigned-int void*))
  ;; void glMultiTexImage3DEXT(GLenum texunit, GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glMultiTexImage3DEXT (unsigned-int unsigned-int int int int int int int unsigned-int unsigned-int void*))
  ;; void glMultiTexSubImage3DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels)
  (define-cdecl void glMultiTexSubImage3DEXT (unsigned-int unsigned-int int int int int int int int unsigned-int unsigned-int void*))
  ;; void glCopyMultiTexSubImage3DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glCopyMultiTexSubImage3DEXT (unsigned-int unsigned-int int int int int int int int int))
  ;; void glEnableClientStateIndexedEXT(GLenum array, GLuint index)
  (define-cdecl void glEnableClientStateIndexedEXT (unsigned-int unsigned-int))
  ;; void glDisableClientStateIndexedEXT(GLenum array, GLuint index)
  (define-cdecl void glDisableClientStateIndexedEXT (unsigned-int unsigned-int))
  ;; void glGetFloatIndexedvEXT(GLenum target, GLuint index, GLfloat *data)
  (define-cdecl void glGetFloatIndexedvEXT (unsigned-int unsigned-int void*))
  ;; void glGetDoubleIndexedvEXT(GLenum target, GLuint index, GLdouble *data)
  (define-cdecl void glGetDoubleIndexedvEXT (unsigned-int unsigned-int void*))
  ;; void glGetPointerIndexedvEXT(GLenum target, GLuint index, void **data)
  (define-cdecl void glGetPointerIndexedvEXT (unsigned-int unsigned-int void*))
  ;; void glEnableIndexedEXT(GLenum target, GLuint index)
  (define-cdecl void glEnableIndexedEXT (unsigned-int unsigned-int))
  ;; void glDisableIndexedEXT(GLenum target, GLuint index)
  (define-cdecl void glDisableIndexedEXT (unsigned-int unsigned-int))
  ;; GLboolean glIsEnabledIndexedEXT(GLenum target, GLuint index)
  (define-cdecl uint8_t glIsEnabledIndexedEXT (unsigned-int unsigned-int))
  ;; void glGetIntegerIndexedvEXT(GLenum target, GLuint index, GLint *data)
  (define-cdecl void glGetIntegerIndexedvEXT (unsigned-int unsigned-int void*))
  ;; void glGetBooleanIndexedvEXT(GLenum target, GLuint index, GLboolean *data)
  (define-cdecl void glGetBooleanIndexedvEXT (unsigned-int unsigned-int void*))
  ;; void glCompressedTextureImage3DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedTextureImage3DEXT (unsigned-int unsigned-int int unsigned-int int int int int int void*))
  ;; void glCompressedTextureImage2DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedTextureImage2DEXT (unsigned-int unsigned-int int unsigned-int int int int int void*))
  ;; void glCompressedTextureImage1DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedTextureImage1DEXT (unsigned-int unsigned-int int unsigned-int int int int void*))
  ;; void glCompressedTextureSubImage3DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedTextureSubImage3DEXT (unsigned-int unsigned-int int int int int int int int unsigned-int int void*))
  ;; void glCompressedTextureSubImage2DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedTextureSubImage2DEXT (unsigned-int unsigned-int int int int int int unsigned-int int void*))
  ;; void glCompressedTextureSubImage1DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedTextureSubImage1DEXT (unsigned-int unsigned-int int int int unsigned-int int void*))
  ;; void glGetCompressedTextureImageEXT(GLuint texture, GLenum target, GLint lod, void *img)
  (define-cdecl void glGetCompressedTextureImageEXT (unsigned-int unsigned-int int void*))
  ;; void glCompressedMultiTexImage3DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedMultiTexImage3DEXT (unsigned-int unsigned-int int unsigned-int int int int int int void*))
  ;; void glCompressedMultiTexImage2DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedMultiTexImage2DEXT (unsigned-int unsigned-int int unsigned-int int int int int void*))
  ;; void glCompressedMultiTexImage1DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedMultiTexImage1DEXT (unsigned-int unsigned-int int unsigned-int int int int void*))
  ;; void glCompressedMultiTexSubImage3DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedMultiTexSubImage3DEXT (unsigned-int unsigned-int int int int int int int int unsigned-int int void*))
  ;; void glCompressedMultiTexSubImage2DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedMultiTexSubImage2DEXT (unsigned-int unsigned-int int int int int int unsigned-int int void*))
  ;; void glCompressedMultiTexSubImage1DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const void *bits)
  (define-cdecl void glCompressedMultiTexSubImage1DEXT (unsigned-int unsigned-int int int int unsigned-int int void*))
  ;; void glGetCompressedMultiTexImageEXT(GLenum texunit, GLenum target, GLint lod, void *img)
  (define-cdecl void glGetCompressedMultiTexImageEXT (unsigned-int unsigned-int int void*))
  ;; void glMatrixLoadTransposefEXT(GLenum mode, const GLfloat *m)
  (define-cdecl void glMatrixLoadTransposefEXT (unsigned-int void*))
  ;; void glMatrixLoadTransposedEXT(GLenum mode, const GLdouble *m)
  (define-cdecl void glMatrixLoadTransposedEXT (unsigned-int void*))
  ;; void glMatrixMultTransposefEXT(GLenum mode, const GLfloat *m)
  (define-cdecl void glMatrixMultTransposefEXT (unsigned-int void*))
  ;; void glMatrixMultTransposedEXT(GLenum mode, const GLdouble *m)
  (define-cdecl void glMatrixMultTransposedEXT (unsigned-int void*))
  ;; void glNamedBufferDataEXT(GLuint buffer, GLsizeiptr size, const void *data, GLenum usage)
  (define-cdecl void glNamedBufferDataEXT (unsigned-int int void* unsigned-int))
  ;; void glNamedBufferSubDataEXT(GLuint buffer, GLintptr offset, GLsizeiptr size, const void *data)
  (define-cdecl void glNamedBufferSubDataEXT (unsigned-int int int void*))
  ;; GLboolean glUnmapNamedBufferEXT(GLuint buffer)
  (define-cdecl uint8_t glUnmapNamedBufferEXT (unsigned-int))
  ;; void glGetNamedBufferParameterivEXT(GLuint buffer, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedBufferParameterivEXT (unsigned-int unsigned-int void*))
  ;; void glGetNamedBufferPointervEXT(GLuint buffer, GLenum pname, void **params)
  (define-cdecl void glGetNamedBufferPointervEXT (unsigned-int unsigned-int void*))
  ;; void glGetNamedBufferSubDataEXT(GLuint buffer, GLintptr offset, GLsizeiptr size, void *data)
  (define-cdecl void glGetNamedBufferSubDataEXT (unsigned-int int int void*))
  ;; void glProgramUniform1fEXT(GLuint program, GLint location, GLfloat v0)
  (define-cdecl void glProgramUniform1fEXT (unsigned-int int float))
  ;; void glProgramUniform2fEXT(GLuint program, GLint location, GLfloat v0, GLfloat v1)
  (define-cdecl void glProgramUniform2fEXT (unsigned-int int float float))
  ;; void glProgramUniform3fEXT(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2)
  (define-cdecl void glProgramUniform3fEXT (unsigned-int int float float float))
  ;; void glProgramUniform4fEXT(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)
  (define-cdecl void glProgramUniform4fEXT (unsigned-int int float float float float))
  ;; void glProgramUniform1iEXT(GLuint program, GLint location, GLint v0)
  (define-cdecl void glProgramUniform1iEXT (unsigned-int int int))
  ;; void glProgramUniform2iEXT(GLuint program, GLint location, GLint v0, GLint v1)
  (define-cdecl void glProgramUniform2iEXT (unsigned-int int int int))
  ;; void glProgramUniform3iEXT(GLuint program, GLint location, GLint v0, GLint v1, GLint v2)
  (define-cdecl void glProgramUniform3iEXT (unsigned-int int int int int))
  ;; void glProgramUniform4iEXT(GLuint program, GLint location, GLint v0, GLint v1, GLint v2, GLint v3)
  (define-cdecl void glProgramUniform4iEXT (unsigned-int int int int int int))
  ;; void glProgramUniform1fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform1fvEXT (unsigned-int int int void*))
  ;; void glProgramUniform2fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform2fvEXT (unsigned-int int int void*))
  ;; void glProgramUniform3fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform3fvEXT (unsigned-int int int void*))
  ;; void glProgramUniform4fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)
  (define-cdecl void glProgramUniform4fvEXT (unsigned-int int int void*))
  ;; void glProgramUniform1ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform1ivEXT (unsigned-int int int void*))
  ;; void glProgramUniform2ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform2ivEXT (unsigned-int int int void*))
  ;; void glProgramUniform3ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform3ivEXT (unsigned-int int int void*))
  ;; void glProgramUniform4ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)
  (define-cdecl void glProgramUniform4ivEXT (unsigned-int int int void*))
  ;; void glProgramUniformMatrix2fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix2fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix3fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix4fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x3fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix2x3fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x2fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix3x2fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x4fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix2x4fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x2fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix4x2fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x4fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix3x4fvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x3fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
  (define-cdecl void glProgramUniformMatrix4x3fvEXT (unsigned-int int int uint8_t void*))
  ;; void glTextureBufferEXT(GLuint texture, GLenum target, GLenum internalformat, GLuint buffer)
  (define-cdecl void glTextureBufferEXT (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glMultiTexBufferEXT(GLenum texunit, GLenum target, GLenum internalformat, GLuint buffer)
  (define-cdecl void glMultiTexBufferEXT (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glTextureParameterIivEXT(GLuint texture, GLenum target, GLenum pname, const GLint *params)
  (define-cdecl void glTextureParameterIivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glTextureParameterIuivEXT(GLuint texture, GLenum target, GLenum pname, const GLuint *params)
  (define-cdecl void glTextureParameterIuivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetTextureParameterIivEXT(GLuint texture, GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetTextureParameterIivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetTextureParameterIuivEXT(GLuint texture, GLenum target, GLenum pname, GLuint *params)
  (define-cdecl void glGetTextureParameterIuivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexParameterIivEXT(GLenum texunit, GLenum target, GLenum pname, const GLint *params)
  (define-cdecl void glMultiTexParameterIivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glMultiTexParameterIuivEXT(GLenum texunit, GLenum target, GLenum pname, const GLuint *params)
  (define-cdecl void glMultiTexParameterIuivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexParameterIivEXT(GLenum texunit, GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetMultiTexParameterIivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetMultiTexParameterIuivEXT(GLenum texunit, GLenum target, GLenum pname, GLuint *params)
  (define-cdecl void glGetMultiTexParameterIuivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glProgramUniform1uiEXT(GLuint program, GLint location, GLuint v0)
  (define-cdecl void glProgramUniform1uiEXT (unsigned-int int unsigned-int))
  ;; void glProgramUniform2uiEXT(GLuint program, GLint location, GLuint v0, GLuint v1)
  (define-cdecl void glProgramUniform2uiEXT (unsigned-int int unsigned-int unsigned-int))
  ;; void glProgramUniform3uiEXT(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2)
  (define-cdecl void glProgramUniform3uiEXT (unsigned-int int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform4uiEXT(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3)
  (define-cdecl void glProgramUniform4uiEXT (unsigned-int int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform1uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform1uivEXT (unsigned-int int int void*))
  ;; void glProgramUniform2uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform2uivEXT (unsigned-int int int void*))
  ;; void glProgramUniform3uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform3uivEXT (unsigned-int int int void*))
  ;; void glProgramUniform4uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)
  (define-cdecl void glProgramUniform4uivEXT (unsigned-int int int void*))
  ;; void glNamedProgramLocalParameters4fvEXT(GLuint program, GLenum target, GLuint index, GLsizei count, const GLfloat *params)
  (define-cdecl void glNamedProgramLocalParameters4fvEXT (unsigned-int unsigned-int unsigned-int int void*))
  ;; void glNamedProgramLocalParameterI4iEXT(GLuint program, GLenum target, GLuint index, GLint x, GLint y, GLint z, GLint w)
  (define-cdecl void glNamedProgramLocalParameterI4iEXT (unsigned-int unsigned-int unsigned-int int int int int))
  ;; void glNamedProgramLocalParameterI4ivEXT(GLuint program, GLenum target, GLuint index, const GLint *params)
  (define-cdecl void glNamedProgramLocalParameterI4ivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glNamedProgramLocalParametersI4ivEXT(GLuint program, GLenum target, GLuint index, GLsizei count, const GLint *params)
  (define-cdecl void glNamedProgramLocalParametersI4ivEXT (unsigned-int unsigned-int unsigned-int int void*))
  ;; void glNamedProgramLocalParameterI4uiEXT(GLuint program, GLenum target, GLuint index, GLuint x, GLuint y, GLuint z, GLuint w)
  (define-cdecl void glNamedProgramLocalParameterI4uiEXT (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glNamedProgramLocalParameterI4uivEXT(GLuint program, GLenum target, GLuint index, const GLuint *params)
  (define-cdecl void glNamedProgramLocalParameterI4uivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glNamedProgramLocalParametersI4uivEXT(GLuint program, GLenum target, GLuint index, GLsizei count, const GLuint *params)
  (define-cdecl void glNamedProgramLocalParametersI4uivEXT (unsigned-int unsigned-int unsigned-int int void*))
  ;; void glGetNamedProgramLocalParameterIivEXT(GLuint program, GLenum target, GLuint index, GLint *params)
  (define-cdecl void glGetNamedProgramLocalParameterIivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetNamedProgramLocalParameterIuivEXT(GLuint program, GLenum target, GLuint index, GLuint *params)
  (define-cdecl void glGetNamedProgramLocalParameterIuivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glEnableClientStateiEXT(GLenum array, GLuint index)
  (define-cdecl void glEnableClientStateiEXT (unsigned-int unsigned-int))
  ;; void glDisableClientStateiEXT(GLenum array, GLuint index)
  (define-cdecl void glDisableClientStateiEXT (unsigned-int unsigned-int))
  ;; void glNamedProgramStringEXT(GLuint program, GLenum target, GLenum format, GLsizei len, const void *string)
  (define-cdecl void glNamedProgramStringEXT (unsigned-int unsigned-int unsigned-int int void*))
  ;; void glNamedProgramLocalParameter4dEXT(GLuint program, GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
  (define-cdecl void glNamedProgramLocalParameter4dEXT (unsigned-int unsigned-int unsigned-int double double double double))
  ;; void glNamedProgramLocalParameter4dvEXT(GLuint program, GLenum target, GLuint index, const GLdouble *params)
  (define-cdecl void glNamedProgramLocalParameter4dvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glNamedProgramLocalParameter4fEXT(GLuint program, GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
  (define-cdecl void glNamedProgramLocalParameter4fEXT (unsigned-int unsigned-int unsigned-int float float float float))
  ;; void glNamedProgramLocalParameter4fvEXT(GLuint program, GLenum target, GLuint index, const GLfloat *params)
  (define-cdecl void glNamedProgramLocalParameter4fvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetNamedProgramLocalParameterdvEXT(GLuint program, GLenum target, GLuint index, GLdouble *params)
  (define-cdecl void glGetNamedProgramLocalParameterdvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetNamedProgramLocalParameterfvEXT(GLuint program, GLenum target, GLuint index, GLfloat *params)
  (define-cdecl void glGetNamedProgramLocalParameterfvEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetNamedProgramivEXT(GLuint program, GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedProgramivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetNamedProgramStringEXT(GLuint program, GLenum target, GLenum pname, void *string)
  (define-cdecl void glGetNamedProgramStringEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glNamedRenderbufferStorageEXT(GLuint renderbuffer, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glNamedRenderbufferStorageEXT (unsigned-int unsigned-int int int))
  ;; void glGetNamedRenderbufferParameterivEXT(GLuint renderbuffer, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedRenderbufferParameterivEXT (unsigned-int unsigned-int void*))
  ;; void glNamedRenderbufferStorageMultisampleEXT(GLuint renderbuffer, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glNamedRenderbufferStorageMultisampleEXT (unsigned-int int unsigned-int int int))
  ;; void glNamedRenderbufferStorageMultisampleCoverageEXT(GLuint renderbuffer, GLsizei coverageSamples, GLsizei colorSamples, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glNamedRenderbufferStorageMultisampleCoverageEXT (unsigned-int int int unsigned-int int int))
  ;; GLenum glCheckNamedFramebufferStatusEXT(GLuint framebuffer, GLenum target)
  (define-cdecl unsigned-int glCheckNamedFramebufferStatusEXT (unsigned-int unsigned-int))
  ;; void glNamedFramebufferTexture1DEXT(GLuint framebuffer, GLenum attachment, GLenum textarget, GLuint texture, GLint level)
  (define-cdecl void glNamedFramebufferTexture1DEXT (unsigned-int unsigned-int unsigned-int unsigned-int int))
  ;; void glNamedFramebufferTexture2DEXT(GLuint framebuffer, GLenum attachment, GLenum textarget, GLuint texture, GLint level)
  (define-cdecl void glNamedFramebufferTexture2DEXT (unsigned-int unsigned-int unsigned-int unsigned-int int))
  ;; void glNamedFramebufferTexture3DEXT(GLuint framebuffer, GLenum attachment, GLenum textarget, GLuint texture, GLint level, GLint zoffset)
  (define-cdecl void glNamedFramebufferTexture3DEXT (unsigned-int unsigned-int unsigned-int unsigned-int int int))
  ;; void glNamedFramebufferRenderbufferEXT(GLuint framebuffer, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)
  (define-cdecl void glNamedFramebufferRenderbufferEXT (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glGetNamedFramebufferAttachmentParameterivEXT(GLuint framebuffer, GLenum attachment, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedFramebufferAttachmentParameterivEXT (unsigned-int unsigned-int unsigned-int void*))
  ;; void glGenerateTextureMipmapEXT(GLuint texture, GLenum target)
  (define-cdecl void glGenerateTextureMipmapEXT (unsigned-int unsigned-int))
  ;; void glGenerateMultiTexMipmapEXT(GLenum texunit, GLenum target)
  (define-cdecl void glGenerateMultiTexMipmapEXT (unsigned-int unsigned-int))
  ;; void glFramebufferDrawBufferEXT(GLuint framebuffer, GLenum mode)
  (define-cdecl void glFramebufferDrawBufferEXT (unsigned-int unsigned-int))
  ;; void glFramebufferDrawBuffersEXT(GLuint framebuffer, GLsizei n, const GLenum *bufs)
  (define-cdecl void glFramebufferDrawBuffersEXT (unsigned-int int void*))
  ;; void glFramebufferReadBufferEXT(GLuint framebuffer, GLenum mode)
  (define-cdecl void glFramebufferReadBufferEXT (unsigned-int unsigned-int))
  ;; void glGetFramebufferParameterivEXT(GLuint framebuffer, GLenum pname, GLint *params)
  (define-cdecl void glGetFramebufferParameterivEXT (unsigned-int unsigned-int void*))
  ;; void glNamedCopyBufferSubDataEXT(GLuint readBuffer, GLuint writeBuffer, GLintptr readOffset, GLintptr writeOffset, GLsizeiptr size)
  (define-cdecl void glNamedCopyBufferSubDataEXT (unsigned-int unsigned-int int int int))
  ;; void glNamedFramebufferTextureEXT(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level)
  (define-cdecl void glNamedFramebufferTextureEXT (unsigned-int unsigned-int unsigned-int int))
  ;; void glNamedFramebufferTextureLayerEXT(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level, GLint layer)
  (define-cdecl void glNamedFramebufferTextureLayerEXT (unsigned-int unsigned-int unsigned-int int int))
  ;; void glNamedFramebufferTextureFaceEXT(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level, GLenum face)
  (define-cdecl void glNamedFramebufferTextureFaceEXT (unsigned-int unsigned-int unsigned-int int unsigned-int))
  ;; void glTextureRenderbufferEXT(GLuint texture, GLenum target, GLuint renderbuffer)
  (define-cdecl void glTextureRenderbufferEXT (unsigned-int unsigned-int unsigned-int))
  ;; void glMultiTexRenderbufferEXT(GLenum texunit, GLenum target, GLuint renderbuffer)
  (define-cdecl void glMultiTexRenderbufferEXT (unsigned-int unsigned-int unsigned-int))
  ;; void glVertexArrayVertexOffsetEXT(GLuint vaobj, GLuint buffer, GLint size, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayVertexOffsetEXT (unsigned-int unsigned-int int unsigned-int int int))
  ;; void glVertexArrayColorOffsetEXT(GLuint vaobj, GLuint buffer, GLint size, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayColorOffsetEXT (unsigned-int unsigned-int int unsigned-int int int))
  ;; void glVertexArrayEdgeFlagOffsetEXT(GLuint vaobj, GLuint buffer, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayEdgeFlagOffsetEXT (unsigned-int unsigned-int int int))
  ;; void glVertexArrayIndexOffsetEXT(GLuint vaobj, GLuint buffer, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayIndexOffsetEXT (unsigned-int unsigned-int unsigned-int int int))
  ;; void glVertexArrayNormalOffsetEXT(GLuint vaobj, GLuint buffer, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayNormalOffsetEXT (unsigned-int unsigned-int unsigned-int int int))
  ;; void glVertexArrayTexCoordOffsetEXT(GLuint vaobj, GLuint buffer, GLint size, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayTexCoordOffsetEXT (unsigned-int unsigned-int int unsigned-int int int))
  ;; void glVertexArrayMultiTexCoordOffsetEXT(GLuint vaobj, GLuint buffer, GLenum texunit, GLint size, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayMultiTexCoordOffsetEXT (unsigned-int unsigned-int unsigned-int int unsigned-int int int))
  ;; void glVertexArrayFogCoordOffsetEXT(GLuint vaobj, GLuint buffer, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayFogCoordOffsetEXT (unsigned-int unsigned-int unsigned-int int int))
  ;; void glVertexArraySecondaryColorOffsetEXT(GLuint vaobj, GLuint buffer, GLint size, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArraySecondaryColorOffsetEXT (unsigned-int unsigned-int int unsigned-int int int))
  ;; void glVertexArrayVertexAttribOffsetEXT(GLuint vaobj, GLuint buffer, GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayVertexAttribOffsetEXT (unsigned-int unsigned-int unsigned-int int unsigned-int uint8_t int int))
  ;; void glVertexArrayVertexAttribIOffsetEXT(GLuint vaobj, GLuint buffer, GLuint index, GLint size, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayVertexAttribIOffsetEXT (unsigned-int unsigned-int unsigned-int int unsigned-int int int))
  ;; void glEnableVertexArrayEXT(GLuint vaobj, GLenum array)
  (define-cdecl void glEnableVertexArrayEXT (unsigned-int unsigned-int))
  ;; void glDisableVertexArrayEXT(GLuint vaobj, GLenum array)
  (define-cdecl void glDisableVertexArrayEXT (unsigned-int unsigned-int))
  ;; void glEnableVertexArrayAttribEXT(GLuint vaobj, GLuint index)
  (define-cdecl void glEnableVertexArrayAttribEXT (unsigned-int unsigned-int))
  ;; void glDisableVertexArrayAttribEXT(GLuint vaobj, GLuint index)
  (define-cdecl void glDisableVertexArrayAttribEXT (unsigned-int unsigned-int))
  ;; void glGetVertexArrayIntegervEXT(GLuint vaobj, GLenum pname, GLint *param)
  (define-cdecl void glGetVertexArrayIntegervEXT (unsigned-int unsigned-int void*))
  ;; void glGetVertexArrayPointervEXT(GLuint vaobj, GLenum pname, void **param)
  (define-cdecl void glGetVertexArrayPointervEXT (unsigned-int unsigned-int void*))
  ;; void glFlushMappedNamedBufferRangeEXT(GLuint buffer, GLintptr offset, GLsizeiptr length)
  (define-cdecl void glFlushMappedNamedBufferRangeEXT (unsigned-int int int))
  ;; void glNamedBufferStorageEXT(GLuint buffer, GLsizeiptr size, const void *data, GLbitfield flags)
  (define-cdecl void glNamedBufferStorageEXT (unsigned-int int void* unsigned-int))
  ;; void glClearNamedBufferDataEXT(GLuint buffer, GLenum internalformat, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearNamedBufferDataEXT (unsigned-int unsigned-int unsigned-int unsigned-int void*))
  ;; void glClearNamedBufferSubDataEXT(GLuint buffer, GLenum internalformat, GLsizeiptr offset, GLsizeiptr size, GLenum format, GLenum type, const void *data)
  (define-cdecl void glClearNamedBufferSubDataEXT (unsigned-int unsigned-int int int unsigned-int unsigned-int void*))
  ;; void glNamedFramebufferParameteriEXT(GLuint framebuffer, GLenum pname, GLint param)
  (define-cdecl void glNamedFramebufferParameteriEXT (unsigned-int unsigned-int int))
  ;; void glGetNamedFramebufferParameterivEXT(GLuint framebuffer, GLenum pname, GLint *params)
  (define-cdecl void glGetNamedFramebufferParameterivEXT (unsigned-int unsigned-int void*))
  ;; void glProgramUniform1dEXT(GLuint program, GLint location, GLdouble x)
  (define-cdecl void glProgramUniform1dEXT (unsigned-int int double))
  ;; void glProgramUniform2dEXT(GLuint program, GLint location, GLdouble x, GLdouble y)
  (define-cdecl void glProgramUniform2dEXT (unsigned-int int double double))
  ;; void glProgramUniform3dEXT(GLuint program, GLint location, GLdouble x, GLdouble y, GLdouble z)
  (define-cdecl void glProgramUniform3dEXT (unsigned-int int double double double))
  ;; void glProgramUniform4dEXT(GLuint program, GLint location, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
  (define-cdecl void glProgramUniform4dEXT (unsigned-int int double double double double))
  ;; void glProgramUniform1dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform1dvEXT (unsigned-int int int void*))
  ;; void glProgramUniform2dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform2dvEXT (unsigned-int int int void*))
  ;; void glProgramUniform3dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform3dvEXT (unsigned-int int int void*))
  ;; void glProgramUniform4dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)
  (define-cdecl void glProgramUniform4dvEXT (unsigned-int int int void*))
  ;; void glProgramUniformMatrix2dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix2dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix3dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix4dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x3dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix2x3dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix2x4dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix2x4dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x2dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix3x2dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix3x4dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix3x4dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x2dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix4x2dvEXT (unsigned-int int int uint8_t void*))
  ;; void glProgramUniformMatrix4x3dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)
  (define-cdecl void glProgramUniformMatrix4x3dvEXT (unsigned-int int int uint8_t void*))
  ;; void glTextureBufferRangeEXT(GLuint texture, GLenum target, GLenum internalformat, GLuint buffer, GLintptr offset, GLsizeiptr size)
  (define-cdecl void glTextureBufferRangeEXT (unsigned-int unsigned-int unsigned-int unsigned-int int int))
  ;; void glTextureStorage1DEXT(GLuint texture, GLenum target, GLsizei levels, GLenum internalformat, GLsizei width)
  (define-cdecl void glTextureStorage1DEXT (unsigned-int unsigned-int int unsigned-int int))
  ;; void glTextureStorage2DEXT(GLuint texture, GLenum target, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glTextureStorage2DEXT (unsigned-int unsigned-int int unsigned-int int int))
  ;; void glTextureStorage3DEXT(GLuint texture, GLenum target, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth)
  (define-cdecl void glTextureStorage3DEXT (unsigned-int unsigned-int int unsigned-int int int int))
  ;; void glTextureStorage2DMultisampleEXT(GLuint texture, GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLboolean fixedsamplelocations)
  (define-cdecl void glTextureStorage2DMultisampleEXT (unsigned-int unsigned-int int unsigned-int int int uint8_t))
  ;; void glTextureStorage3DMultisampleEXT(GLuint texture, GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean fixedsamplelocations)
  (define-cdecl void glTextureStorage3DMultisampleEXT (unsigned-int unsigned-int int unsigned-int int int int uint8_t))
  ;; void glVertexArrayBindVertexBufferEXT(GLuint vaobj, GLuint bindingindex, GLuint buffer, GLintptr offset, GLsizei stride)
  (define-cdecl void glVertexArrayBindVertexBufferEXT (unsigned-int unsigned-int unsigned-int int int))
  ;; void glVertexArrayVertexAttribFormatEXT(GLuint vaobj, GLuint attribindex, GLint size, GLenum type, GLboolean normalized, GLuint relativeoffset)
  (define-cdecl void glVertexArrayVertexAttribFormatEXT (unsigned-int unsigned-int int unsigned-int uint8_t unsigned-int))
  ;; void glVertexArrayVertexAttribIFormatEXT(GLuint vaobj, GLuint attribindex, GLint size, GLenum type, GLuint relativeoffset)
  (define-cdecl void glVertexArrayVertexAttribIFormatEXT (unsigned-int unsigned-int int unsigned-int unsigned-int))
  ;; void glVertexArrayVertexAttribLFormatEXT(GLuint vaobj, GLuint attribindex, GLint size, GLenum type, GLuint relativeoffset)
  (define-cdecl void glVertexArrayVertexAttribLFormatEXT (unsigned-int unsigned-int int unsigned-int unsigned-int))
  ;; void glVertexArrayVertexAttribBindingEXT(GLuint vaobj, GLuint attribindex, GLuint bindingindex)
  (define-cdecl void glVertexArrayVertexAttribBindingEXT (unsigned-int unsigned-int unsigned-int))
  ;; void glVertexArrayVertexBindingDivisorEXT(GLuint vaobj, GLuint bindingindex, GLuint divisor)
  (define-cdecl void glVertexArrayVertexBindingDivisorEXT (unsigned-int unsigned-int unsigned-int))
  ;; void glVertexArrayVertexAttribLOffsetEXT(GLuint vaobj, GLuint buffer, GLuint index, GLint size, GLenum type, GLsizei stride, GLintptr offset)
  (define-cdecl void glVertexArrayVertexAttribLOffsetEXT (unsigned-int unsigned-int unsigned-int int unsigned-int int int))
  ;; void glTexturePageCommitmentEXT(GLuint texture, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLboolean commit)
  (define-cdecl void glTexturePageCommitmentEXT (unsigned-int int int int int int int int uint8_t))
  ;; void glVertexArrayVertexAttribDivisorEXT(GLuint vaobj, GLuint index, GLuint divisor)
  (define-cdecl void glVertexArrayVertexAttribDivisorEXT (unsigned-int unsigned-int unsigned-int))
  ;; void glDrawArraysInstancedEXT(GLenum mode, GLint start, GLsizei count, GLsizei primcount)
  (define-cdecl void glDrawArraysInstancedEXT (unsigned-int int int int))
  ;; void glDrawElementsInstancedEXT(GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei primcount)
  (define-cdecl void glDrawElementsInstancedEXT (unsigned-int int unsigned-int void* int))
  ;; void glPolygonOffsetClampEXT(GLfloat factor, GLfloat units, GLfloat clamp)
  (define-cdecl void glPolygonOffsetClampEXT (float float float))
  ;; void glRasterSamplesEXT(GLuint samples, GLboolean fixedsamplelocations)
  (define-cdecl void glRasterSamplesEXT (unsigned-int uint8_t))
  ;; void glUseShaderProgramEXT(GLenum type, GLuint program)
  (define-cdecl void glUseShaderProgramEXT (unsigned-int unsigned-int))
  ;; void glActiveProgramEXT(GLuint program)
  (define-cdecl void glActiveProgramEXT (unsigned-int))
  ;; GLuint glCreateShaderProgramEXT(GLenum type, const GLchar *string)
  (define-cdecl unsigned-int glCreateShaderProgramEXT (unsigned-int void*))
  ;; void glFramebufferFetchBarrierEXT(void)
  (define-cdecl void glFramebufferFetchBarrierEXT ())
  ;; void glWindowRectanglesEXT(GLenum mode, GLsizei count, const GLint *box)
  (define-cdecl void glWindowRectanglesEXT (unsigned-int int void*))
  ;; void glApplyFramebufferAttachmentCMAAINTEL(void)
  (define-cdecl void glApplyFramebufferAttachmentCMAAINTEL ())
  ;; void glBeginPerfQueryINTEL(GLuint queryHandle)
  (define-cdecl void glBeginPerfQueryINTEL (unsigned-int))
  ;; void glCreatePerfQueryINTEL(GLuint queryId, GLuint *queryHandle)
  (define-cdecl void glCreatePerfQueryINTEL (unsigned-int void*))
  ;; void glDeletePerfQueryINTEL(GLuint queryHandle)
  (define-cdecl void glDeletePerfQueryINTEL (unsigned-int))
  ;; void glEndPerfQueryINTEL(GLuint queryHandle)
  (define-cdecl void glEndPerfQueryINTEL (unsigned-int))
  ;; void glGetFirstPerfQueryIdINTEL(GLuint *queryId)
  (define-cdecl void glGetFirstPerfQueryIdINTEL (void*))
  ;; void glGetNextPerfQueryIdINTEL(GLuint queryId, GLuint *nextQueryId)
  (define-cdecl void glGetNextPerfQueryIdINTEL (unsigned-int void*))
  ;; void glGetPerfCounterInfoINTEL(GLuint queryId, GLuint counterId, GLuint counterNameLength, GLchar *counterName, GLuint counterDescLength, GLchar *counterDesc, GLuint *counterOffset, GLuint *counterDataSize, GLuint *counterTypeEnum, GLuint *counterDataTypeEnum, GLuint64 *rawCounterMaxValue)
  (define-cdecl void glGetPerfCounterInfoINTEL (unsigned-int unsigned-int unsigned-int void* unsigned-int void* void* void* void* void* void*))
  ;; void glGetPerfQueryDataINTEL(GLuint queryHandle, GLuint flags, GLsizei dataSize, void *data, GLuint *bytesWritten)
  (define-cdecl void glGetPerfQueryDataINTEL (unsigned-int unsigned-int int void* void*))
  ;; void glGetPerfQueryIdByNameINTEL(GLchar *queryName, GLuint *queryId)
  (define-cdecl void glGetPerfQueryIdByNameINTEL (void* void*))
  ;; void glGetPerfQueryInfoINTEL(GLuint queryId, GLuint queryNameLength, GLchar *queryName, GLuint *dataSize, GLuint *noCounters, GLuint *noInstances, GLuint *capsMask)
  (define-cdecl void glGetPerfQueryInfoINTEL (unsigned-int unsigned-int void* void* void* void* void*))
  ;; void glFramebufferParameteriMESA(GLenum target, GLenum pname, GLint param)
  (define-cdecl void glFramebufferParameteriMESA (unsigned-int unsigned-int int))
  ;; void glGetFramebufferParameterivMESA(GLenum target, GLenum pname, GLint *params)
  (define-cdecl void glGetFramebufferParameterivMESA (unsigned-int unsigned-int void*))
  ;; void glMultiDrawArraysIndirectBindlessNV(GLenum mode, const void *indirect, GLsizei drawCount, GLsizei stride, GLint vertexBufferCount)
  (define-cdecl void glMultiDrawArraysIndirectBindlessNV (unsigned-int void* int int int))
  ;; void glMultiDrawElementsIndirectBindlessNV(GLenum mode, GLenum type, const void *indirect, GLsizei drawCount, GLsizei stride, GLint vertexBufferCount)
  (define-cdecl void glMultiDrawElementsIndirectBindlessNV (unsigned-int unsigned-int void* int int int))
  ;; void glMultiDrawArraysIndirectBindlessCountNV(GLenum mode, const void *indirect, GLsizei drawCount, GLsizei maxDrawCount, GLsizei stride, GLint vertexBufferCount)
  (define-cdecl void glMultiDrawArraysIndirectBindlessCountNV (unsigned-int void* int int int int))
  ;; void glMultiDrawElementsIndirectBindlessCountNV(GLenum mode, GLenum type, const void *indirect, GLsizei drawCount, GLsizei maxDrawCount, GLsizei stride, GLint vertexBufferCount)
  (define-cdecl void glMultiDrawElementsIndirectBindlessCountNV (unsigned-int unsigned-int void* int int int int))
  ;; GLuint64 glGetTextureHandleNV(GLuint texture)
  (define-cdecl unsigned-int glGetTextureHandleNV (unsigned-int))
  ;; GLuint64 glGetTextureSamplerHandleNV(GLuint texture, GLuint sampler)
  (define-cdecl unsigned-int glGetTextureSamplerHandleNV (unsigned-int unsigned-int))
  ;; void glMakeTextureHandleResidentNV(GLuint64 handle)
  (define-cdecl void glMakeTextureHandleResidentNV (unsigned-int))
  ;; void glMakeTextureHandleNonResidentNV(GLuint64 handle)
  (define-cdecl void glMakeTextureHandleNonResidentNV (unsigned-int))
  ;; GLuint64 glGetImageHandleNV(GLuint texture, GLint level, GLboolean layered, GLint layer, GLenum format)
  (define-cdecl unsigned-int glGetImageHandleNV (unsigned-int int uint8_t int unsigned-int))
  ;; void glMakeImageHandleResidentNV(GLuint64 handle, GLenum access)
  (define-cdecl void glMakeImageHandleResidentNV (unsigned-int unsigned-int))
  ;; void glMakeImageHandleNonResidentNV(GLuint64 handle)
  (define-cdecl void glMakeImageHandleNonResidentNV (unsigned-int))
  ;; void glUniformHandleui64NV(GLint location, GLuint64 value)
  (define-cdecl void glUniformHandleui64NV (int unsigned-int))
  ;; void glUniformHandleui64vNV(GLint location, GLsizei count, const GLuint64 *value)
  (define-cdecl void glUniformHandleui64vNV (int int void*))
  ;; void glProgramUniformHandleui64NV(GLuint program, GLint location, GLuint64 value)
  (define-cdecl void glProgramUniformHandleui64NV (unsigned-int int unsigned-int))
  ;; void glProgramUniformHandleui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64 *values)
  (define-cdecl void glProgramUniformHandleui64vNV (unsigned-int int int void*))
  ;; GLboolean glIsTextureHandleResidentNV(GLuint64 handle)
  (define-cdecl uint8_t glIsTextureHandleResidentNV (unsigned-int))
  ;; GLboolean glIsImageHandleResidentNV(GLuint64 handle)
  (define-cdecl uint8_t glIsImageHandleResidentNV (unsigned-int))
  ;; void glBlendParameteriNV(GLenum pname, GLint value)
  (define-cdecl void glBlendParameteriNV (unsigned-int int))
  ;; void glBlendBarrierNV(void)
  (define-cdecl void glBlendBarrierNV ())
  ;; void glViewportPositionWScaleNV(GLuint index, GLfloat xcoeff, GLfloat ycoeff)
  (define-cdecl void glViewportPositionWScaleNV (unsigned-int float float))
  ;; void glCreateStatesNV(GLsizei n, GLuint *states)
  (define-cdecl void glCreateStatesNV (int void*))
  ;; void glDeleteStatesNV(GLsizei n, const GLuint *states)
  (define-cdecl void glDeleteStatesNV (int void*))
  ;; GLboolean glIsStateNV(GLuint state)
  (define-cdecl uint8_t glIsStateNV (unsigned-int))
  ;; void glStateCaptureNV(GLuint state, GLenum mode)
  (define-cdecl void glStateCaptureNV (unsigned-int unsigned-int))
  ;; GLuint glGetCommandHeaderNV(GLenum tokenID, GLuint size)
  (define-cdecl unsigned-int glGetCommandHeaderNV (unsigned-int unsigned-int))
  ;; GLushort glGetStageIndexNV(GLenum shadertype)
  (define-cdecl unsigned-short glGetStageIndexNV (unsigned-int))
  ;; void glDrawCommandsNV(GLenum primitiveMode, GLuint buffer, const GLintptr *indirects, const GLsizei *sizes, GLuint count)
  (define-cdecl void glDrawCommandsNV (unsigned-int unsigned-int void* void* unsigned-int))
  ;; void glDrawCommandsAddressNV(GLenum primitiveMode, const GLuint64 *indirects, const GLsizei *sizes, GLuint count)
  (define-cdecl void glDrawCommandsAddressNV (unsigned-int void* void* unsigned-int))
  ;; void glDrawCommandsStatesNV(GLuint buffer, const GLintptr *indirects, const GLsizei *sizes, const GLuint *states, const GLuint *fbos, GLuint count)
  (define-cdecl void glDrawCommandsStatesNV (unsigned-int void* void* void* void* unsigned-int))
  ;; void glDrawCommandsStatesAddressNV(const GLuint64 *indirects, const GLsizei *sizes, const GLuint *states, const GLuint *fbos, GLuint count)
  (define-cdecl void glDrawCommandsStatesAddressNV (void* void* void* void* unsigned-int))
  ;; void glCreateCommandListsNV(GLsizei n, GLuint *lists)
  (define-cdecl void glCreateCommandListsNV (int void*))
  ;; void glDeleteCommandListsNV(GLsizei n, const GLuint *lists)
  (define-cdecl void glDeleteCommandListsNV (int void*))
  ;; GLboolean glIsCommandListNV(GLuint list)
  (define-cdecl uint8_t glIsCommandListNV (unsigned-int))
  ;; void glListDrawCommandsStatesClientNV(GLuint list, GLuint segment, const void **indirects, const GLsizei *sizes, const GLuint *states, const GLuint *fbos, GLuint count)
  (define-cdecl void glListDrawCommandsStatesClientNV (unsigned-int unsigned-int void* void* void* void* unsigned-int))
  ;; void glCommandListSegmentsNV(GLuint list, GLuint segments)
  (define-cdecl void glCommandListSegmentsNV (unsigned-int unsigned-int))
  ;; void glCompileCommandListNV(GLuint list)
  (define-cdecl void glCompileCommandListNV (unsigned-int))
  ;; void glCallCommandListNV(GLuint list)
  (define-cdecl void glCallCommandListNV (unsigned-int))
  ;; void glBeginConditionalRenderNV(GLuint id, GLenum mode)
  (define-cdecl void glBeginConditionalRenderNV (unsigned-int unsigned-int))
  ;; void glEndConditionalRenderNV(void)
  (define-cdecl void glEndConditionalRenderNV ())
  ;; void glSubpixelPrecisionBiasNV(GLuint xbits, GLuint ybits)
  (define-cdecl void glSubpixelPrecisionBiasNV (unsigned-int unsigned-int))
  ;; void glConservativeRasterParameterfNV(GLenum pname, GLfloat value)
  (define-cdecl void glConservativeRasterParameterfNV (unsigned-int float))
  ;; void glConservativeRasterParameteriNV(GLenum pname, GLint param)
  (define-cdecl void glConservativeRasterParameteriNV (unsigned-int int))
  ;; void glDepthRangedNV(GLdouble zNear, GLdouble zFar)
  (define-cdecl void glDepthRangedNV (double double))
  ;; void glClearDepthdNV(GLdouble depth)
  (define-cdecl void glClearDepthdNV (double))
  ;; void glDepthBoundsdNV(GLdouble zmin, GLdouble zmax)
  (define-cdecl void glDepthBoundsdNV (double double))
  ;; void glDrawVkImageNV(GLuint64 vkImage, GLuint sampler, GLfloat x0, GLfloat y0, GLfloat x1, GLfloat y1, GLfloat z, GLfloat s0, GLfloat t0, GLfloat s1, GLfloat t1)
  (define-cdecl void glDrawVkImageNV (unsigned-int unsigned-int float float float float float float float float float))
  ;; GLVULKANPROCNV glGetVkProcAddrNV(const GLchar *name)
  (define-cdecl void* glGetVkProcAddrNV (void*))
  ;; void glWaitVkSemaphoreNV(GLuint64 vkSemaphore)
  (define-cdecl void glWaitVkSemaphoreNV (unsigned-int))
  ;; void glSignalVkSemaphoreNV(GLuint64 vkSemaphore)
  (define-cdecl void glSignalVkSemaphoreNV (unsigned-int))
  ;; void glSignalVkFenceNV(GLuint64 vkFence)
  (define-cdecl void glSignalVkFenceNV (unsigned-int))
  ;; void glFragmentCoverageColorNV(GLuint color)
  (define-cdecl void glFragmentCoverageColorNV (unsigned-int))
  ;; void glCoverageModulationTableNV(GLsizei n, const GLfloat *v)
  (define-cdecl void glCoverageModulationTableNV (int void*))
  ;; void glGetCoverageModulationTableNV(GLsizei bufSize, GLfloat *v)
  (define-cdecl void glGetCoverageModulationTableNV (int void*))
  ;; void glCoverageModulationNV(GLenum components)
  (define-cdecl void glCoverageModulationNV (unsigned-int))
  ;; void glRenderbufferStorageMultisampleCoverageNV(GLenum target, GLsizei coverageSamples, GLsizei colorSamples, GLenum internalformat, GLsizei width, GLsizei height)
  (define-cdecl void glRenderbufferStorageMultisampleCoverageNV (unsigned-int int int unsigned-int int int))
  ;; void glUniform1i64NV(GLint location, GLint64EXT x)
  (define-cdecl void glUniform1i64NV (int int))
  ;; void glUniform2i64NV(GLint location, GLint64EXT x, GLint64EXT y)
  (define-cdecl void glUniform2i64NV (int int int))
  ;; void glUniform3i64NV(GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z)
  (define-cdecl void glUniform3i64NV (int int int int))
  ;; void glUniform4i64NV(GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z, GLint64EXT w)
  (define-cdecl void glUniform4i64NV (int int int int int))
  ;; void glUniform1i64vNV(GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glUniform1i64vNV (int int void*))
  ;; void glUniform2i64vNV(GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glUniform2i64vNV (int int void*))
  ;; void glUniform3i64vNV(GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glUniform3i64vNV (int int void*))
  ;; void glUniform4i64vNV(GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glUniform4i64vNV (int int void*))
  ;; void glUniform1ui64NV(GLint location, GLuint64EXT x)
  (define-cdecl void glUniform1ui64NV (int unsigned-int))
  ;; void glUniform2ui64NV(GLint location, GLuint64EXT x, GLuint64EXT y)
  (define-cdecl void glUniform2ui64NV (int unsigned-int unsigned-int))
  ;; void glUniform3ui64NV(GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z)
  (define-cdecl void glUniform3ui64NV (int unsigned-int unsigned-int unsigned-int))
  ;; void glUniform4ui64NV(GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z, GLuint64EXT w)
  (define-cdecl void glUniform4ui64NV (int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glUniform1ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glUniform1ui64vNV (int int void*))
  ;; void glUniform2ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glUniform2ui64vNV (int int void*))
  ;; void glUniform3ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glUniform3ui64vNV (int int void*))
  ;; void glUniform4ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glUniform4ui64vNV (int int void*))
  ;; void glGetUniformi64vNV(GLuint program, GLint location, GLint64EXT *params)
  (define-cdecl void glGetUniformi64vNV (unsigned-int int void*))
  ;; void glProgramUniform1i64NV(GLuint program, GLint location, GLint64EXT x)
  (define-cdecl void glProgramUniform1i64NV (unsigned-int int int))
  ;; void glProgramUniform2i64NV(GLuint program, GLint location, GLint64EXT x, GLint64EXT y)
  (define-cdecl void glProgramUniform2i64NV (unsigned-int int int int))
  ;; void glProgramUniform3i64NV(GLuint program, GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z)
  (define-cdecl void glProgramUniform3i64NV (unsigned-int int int int int))
  ;; void glProgramUniform4i64NV(GLuint program, GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z, GLint64EXT w)
  (define-cdecl void glProgramUniform4i64NV (unsigned-int int int int int int))
  ;; void glProgramUniform1i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glProgramUniform1i64vNV (unsigned-int int int void*))
  ;; void glProgramUniform2i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glProgramUniform2i64vNV (unsigned-int int int void*))
  ;; void glProgramUniform3i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glProgramUniform3i64vNV (unsigned-int int int void*))
  ;; void glProgramUniform4i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)
  (define-cdecl void glProgramUniform4i64vNV (unsigned-int int int void*))
  ;; void glProgramUniform1ui64NV(GLuint program, GLint location, GLuint64EXT x)
  (define-cdecl void glProgramUniform1ui64NV (unsigned-int int unsigned-int))
  ;; void glProgramUniform2ui64NV(GLuint program, GLint location, GLuint64EXT x, GLuint64EXT y)
  (define-cdecl void glProgramUniform2ui64NV (unsigned-int int unsigned-int unsigned-int))
  ;; void glProgramUniform3ui64NV(GLuint program, GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z)
  (define-cdecl void glProgramUniform3ui64NV (unsigned-int int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform4ui64NV(GLuint program, GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z, GLuint64EXT w)
  (define-cdecl void glProgramUniform4ui64NV (unsigned-int int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glProgramUniform1ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glProgramUniform1ui64vNV (unsigned-int int int void*))
  ;; void glProgramUniform2ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glProgramUniform2ui64vNV (unsigned-int int int void*))
  ;; void glProgramUniform3ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glProgramUniform3ui64vNV (unsigned-int int int void*))
  ;; void glProgramUniform4ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glProgramUniform4ui64vNV (unsigned-int int int void*))
  ;; void glGetInternalformatSampleivNV(GLenum target, GLenum internalformat, GLsizei samples, GLenum pname, GLsizei count, GLint *params)
  (define-cdecl void glGetInternalformatSampleivNV (unsigned-int unsigned-int int unsigned-int int void*))
  ;; void glGetMemoryObjectDetachedResourcesuivNV(GLuint memory, GLenum pname, GLint first, GLsizei count, GLuint *params)
  (define-cdecl void glGetMemoryObjectDetachedResourcesuivNV (unsigned-int unsigned-int int int void*))
  ;; void glResetMemoryObjectParameterNV(GLuint memory, GLenum pname)
  (define-cdecl void glResetMemoryObjectParameterNV (unsigned-int unsigned-int))
  ;; void glTexAttachMemoryNV(GLenum target, GLuint memory, GLuint64 offset)
  (define-cdecl void glTexAttachMemoryNV (unsigned-int unsigned-int unsigned-int))
  ;; void glBufferAttachMemoryNV(GLenum target, GLuint memory, GLuint64 offset)
  (define-cdecl void glBufferAttachMemoryNV (unsigned-int unsigned-int unsigned-int))
  ;; void glTextureAttachMemoryNV(GLuint texture, GLuint memory, GLuint64 offset)
  (define-cdecl void glTextureAttachMemoryNV (unsigned-int unsigned-int unsigned-int))
  ;; void glNamedBufferAttachMemoryNV(GLuint buffer, GLuint memory, GLuint64 offset)
  (define-cdecl void glNamedBufferAttachMemoryNV (unsigned-int unsigned-int unsigned-int))
  ;; void glDrawMeshTasksNV(GLuint first, GLuint count)
  (define-cdecl void glDrawMeshTasksNV (unsigned-int unsigned-int))
  ;; void glDrawMeshTasksIndirectNV(GLintptr indirect)
  (define-cdecl void glDrawMeshTasksIndirectNV (int))
  ;; void glMultiDrawMeshTasksIndirectNV(GLintptr indirect, GLsizei drawcount, GLsizei stride)
  (define-cdecl void glMultiDrawMeshTasksIndirectNV (int int int))
  ;; void glMultiDrawMeshTasksIndirectCountNV(GLintptr indirect, GLintptr drawcount, GLsizei maxdrawcount, GLsizei stride)
  (define-cdecl void glMultiDrawMeshTasksIndirectCountNV (int int int int))
  ;; GLuint glGenPathsNV(GLsizei range)
  (define-cdecl unsigned-int glGenPathsNV (int))
  ;; void glDeletePathsNV(GLuint path, GLsizei range)
  (define-cdecl void glDeletePathsNV (unsigned-int int))
  ;; GLboolean glIsPathNV(GLuint path)
  (define-cdecl uint8_t glIsPathNV (unsigned-int))
  ;; void glPathCommandsNV(GLuint path, GLsizei numCommands, const GLubyte *commands, GLsizei numCoords, GLenum coordType, const void *coords)
  (define-cdecl void glPathCommandsNV (unsigned-int int void* int unsigned-int void*))
  ;; void glPathCoordsNV(GLuint path, GLsizei numCoords, GLenum coordType, const void *coords)
  (define-cdecl void glPathCoordsNV (unsigned-int int unsigned-int void*))
  ;; void glPathSubCommandsNV(GLuint path, GLsizei commandStart, GLsizei commandsToDelete, GLsizei numCommands, const GLubyte *commands, GLsizei numCoords, GLenum coordType, const void *coords)
  (define-cdecl void glPathSubCommandsNV (unsigned-int int int int void* int unsigned-int void*))
  ;; void glPathSubCoordsNV(GLuint path, GLsizei coordStart, GLsizei numCoords, GLenum coordType, const void *coords)
  (define-cdecl void glPathSubCoordsNV (unsigned-int int int unsigned-int void*))
  ;; void glPathStringNV(GLuint path, GLenum format, GLsizei length, const void *pathString)
  (define-cdecl void glPathStringNV (unsigned-int unsigned-int int void*))
  ;; void glPathGlyphsNV(GLuint firstPathName, GLenum fontTarget, const void *fontName, GLbitfield fontStyle, GLsizei numGlyphs, GLenum type, const void *charcodes, GLenum handleMissingGlyphs, GLuint pathParameterTemplate, GLfloat emScale)
  (define-cdecl void glPathGlyphsNV (unsigned-int unsigned-int void* unsigned-int int unsigned-int void* unsigned-int unsigned-int float))
  ;; void glPathGlyphRangeNV(GLuint firstPathName, GLenum fontTarget, const void *fontName, GLbitfield fontStyle, GLuint firstGlyph, GLsizei numGlyphs, GLenum handleMissingGlyphs, GLuint pathParameterTemplate, GLfloat emScale)
  (define-cdecl void glPathGlyphRangeNV (unsigned-int unsigned-int void* unsigned-int unsigned-int int unsigned-int unsigned-int float))
  ;; void glWeightPathsNV(GLuint resultPath, GLsizei numPaths, const GLuint *paths, const GLfloat *weights)
  (define-cdecl void glWeightPathsNV (unsigned-int int void* void*))
  ;; void glCopyPathNV(GLuint resultPath, GLuint srcPath)
  (define-cdecl void glCopyPathNV (unsigned-int unsigned-int))
  ;; void glInterpolatePathsNV(GLuint resultPath, GLuint pathA, GLuint pathB, GLfloat weight)
  (define-cdecl void glInterpolatePathsNV (unsigned-int unsigned-int unsigned-int float))
  ;; void glTransformPathNV(GLuint resultPath, GLuint srcPath, GLenum transformType, const GLfloat *transformValues)
  (define-cdecl void glTransformPathNV (unsigned-int unsigned-int unsigned-int void*))
  ;; void glPathParameterivNV(GLuint path, GLenum pname, const GLint *value)
  (define-cdecl void glPathParameterivNV (unsigned-int unsigned-int void*))
  ;; void glPathParameteriNV(GLuint path, GLenum pname, GLint value)
  (define-cdecl void glPathParameteriNV (unsigned-int unsigned-int int))
  ;; void glPathParameterfvNV(GLuint path, GLenum pname, const GLfloat *value)
  (define-cdecl void glPathParameterfvNV (unsigned-int unsigned-int void*))
  ;; void glPathParameterfNV(GLuint path, GLenum pname, GLfloat value)
  (define-cdecl void glPathParameterfNV (unsigned-int unsigned-int float))
  ;; void glPathDashArrayNV(GLuint path, GLsizei dashCount, const GLfloat *dashArray)
  (define-cdecl void glPathDashArrayNV (unsigned-int int void*))
  ;; void glPathStencilFuncNV(GLenum func, GLint ref, GLuint mask)
  (define-cdecl void glPathStencilFuncNV (unsigned-int int unsigned-int))
  ;; void glPathStencilDepthOffsetNV(GLfloat factor, GLfloat units)
  (define-cdecl void glPathStencilDepthOffsetNV (float float))
  ;; void glStencilFillPathNV(GLuint path, GLenum fillMode, GLuint mask)
  (define-cdecl void glStencilFillPathNV (unsigned-int unsigned-int unsigned-int))
  ;; void glStencilStrokePathNV(GLuint path, GLint reference, GLuint mask)
  (define-cdecl void glStencilStrokePathNV (unsigned-int int unsigned-int))
  ;; void glStencilFillPathInstancedNV(GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLenum fillMode, GLuint mask, GLenum transformType, const GLfloat *transformValues)
  (define-cdecl void glStencilFillPathInstancedNV (int unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int void*))
  ;; void glStencilStrokePathInstancedNV(GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLint reference, GLuint mask, GLenum transformType, const GLfloat *transformValues)
  (define-cdecl void glStencilStrokePathInstancedNV (int unsigned-int void* unsigned-int int unsigned-int unsigned-int void*))
  ;; void glPathCoverDepthFuncNV(GLenum func)
  (define-cdecl void glPathCoverDepthFuncNV (unsigned-int))
  ;; void glCoverFillPathNV(GLuint path, GLenum coverMode)
  (define-cdecl void glCoverFillPathNV (unsigned-int unsigned-int))
  ;; void glCoverStrokePathNV(GLuint path, GLenum coverMode)
  (define-cdecl void glCoverStrokePathNV (unsigned-int unsigned-int))
  ;; void glCoverFillPathInstancedNV(GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLenum coverMode, GLenum transformType, const GLfloat *transformValues)
  (define-cdecl void glCoverFillPathInstancedNV (int unsigned-int void* unsigned-int unsigned-int unsigned-int void*))
  ;; void glCoverStrokePathInstancedNV(GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLenum coverMode, GLenum transformType, const GLfloat *transformValues)
  (define-cdecl void glCoverStrokePathInstancedNV (int unsigned-int void* unsigned-int unsigned-int unsigned-int void*))
  ;; void glGetPathParameterivNV(GLuint path, GLenum pname, GLint *value)
  (define-cdecl void glGetPathParameterivNV (unsigned-int unsigned-int void*))
  ;; void glGetPathParameterfvNV(GLuint path, GLenum pname, GLfloat *value)
  (define-cdecl void glGetPathParameterfvNV (unsigned-int unsigned-int void*))
  ;; void glGetPathCommandsNV(GLuint path, GLubyte *commands)
  (define-cdecl void glGetPathCommandsNV (unsigned-int void*))
  ;; void glGetPathCoordsNV(GLuint path, GLfloat *coords)
  (define-cdecl void glGetPathCoordsNV (unsigned-int void*))
  ;; void glGetPathDashArrayNV(GLuint path, GLfloat *dashArray)
  (define-cdecl void glGetPathDashArrayNV (unsigned-int void*))
  ;; void glGetPathMetricsNV(GLbitfield metricQueryMask, GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLsizei stride, GLfloat *metrics)
  (define-cdecl void glGetPathMetricsNV (unsigned-int int unsigned-int void* unsigned-int int void*))
  ;; void glGetPathMetricRangeNV(GLbitfield metricQueryMask, GLuint firstPathName, GLsizei numPaths, GLsizei stride, GLfloat *metrics)
  (define-cdecl void glGetPathMetricRangeNV (unsigned-int unsigned-int int int void*))
  ;; void glGetPathSpacingNV(GLenum pathListMode, GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLfloat advanceScale, GLfloat kerningScale, GLenum transformType, GLfloat *returnedSpacing)
  (define-cdecl void glGetPathSpacingNV (unsigned-int int unsigned-int void* unsigned-int float float unsigned-int void*))
  ;; GLboolean glIsPointInFillPathNV(GLuint path, GLuint mask, GLfloat x, GLfloat y)
  (define-cdecl uint8_t glIsPointInFillPathNV (unsigned-int unsigned-int float float))
  ;; GLboolean glIsPointInStrokePathNV(GLuint path, GLfloat x, GLfloat y)
  (define-cdecl uint8_t glIsPointInStrokePathNV (unsigned-int float float))
  ;; GLfloat glGetPathLengthNV(GLuint path, GLsizei startSegment, GLsizei numSegments)
  (define-cdecl float glGetPathLengthNV (unsigned-int int int))
  ;; GLboolean glPointAlongPathNV(GLuint path, GLsizei startSegment, GLsizei numSegments, GLfloat distance, GLfloat *x, GLfloat *y, GLfloat *tangentX, GLfloat *tangentY)
  (define-cdecl uint8_t glPointAlongPathNV (unsigned-int int int float void* void* void* void*))
  ;; void glMatrixLoad3x2fNV(GLenum matrixMode, const GLfloat *m)
  (define-cdecl void glMatrixLoad3x2fNV (unsigned-int void*))
  ;; void glMatrixLoad3x3fNV(GLenum matrixMode, const GLfloat *m)
  (define-cdecl void glMatrixLoad3x3fNV (unsigned-int void*))
  ;; void glMatrixLoadTranspose3x3fNV(GLenum matrixMode, const GLfloat *m)
  (define-cdecl void glMatrixLoadTranspose3x3fNV (unsigned-int void*))
  ;; void glMatrixMult3x2fNV(GLenum matrixMode, const GLfloat *m)
  (define-cdecl void glMatrixMult3x2fNV (unsigned-int void*))
  ;; void glMatrixMult3x3fNV(GLenum matrixMode, const GLfloat *m)
  (define-cdecl void glMatrixMult3x3fNV (unsigned-int void*))
  ;; void glMatrixMultTranspose3x3fNV(GLenum matrixMode, const GLfloat *m)
  (define-cdecl void glMatrixMultTranspose3x3fNV (unsigned-int void*))
  ;; void glStencilThenCoverFillPathNV(GLuint path, GLenum fillMode, GLuint mask, GLenum coverMode)
  (define-cdecl void glStencilThenCoverFillPathNV (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glStencilThenCoverStrokePathNV(GLuint path, GLint reference, GLuint mask, GLenum coverMode)
  (define-cdecl void glStencilThenCoverStrokePathNV (unsigned-int int unsigned-int unsigned-int))
  ;; void glStencilThenCoverFillPathInstancedNV(GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLenum fillMode, GLuint mask, GLenum coverMode, GLenum transformType, const GLfloat *transformValues)
  (define-cdecl void glStencilThenCoverFillPathInstancedNV (int unsigned-int void* unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int void*))
  ;; void glStencilThenCoverStrokePathInstancedNV(GLsizei numPaths, GLenum pathNameType, const void *paths, GLuint pathBase, GLint reference, GLuint mask, GLenum coverMode, GLenum transformType, const GLfloat *transformValues)
  (define-cdecl void glStencilThenCoverStrokePathInstancedNV (int unsigned-int void* unsigned-int int unsigned-int unsigned-int unsigned-int void*))
  ;; GLenum glPathGlyphIndexRangeNV(GLenum fontTarget, const void *fontName, GLbitfield fontStyle, GLuint pathParameterTemplate, GLfloat emScale, GLuint baseAndCount[2])
  (define-cdecl unsigned-int glPathGlyphIndexRangeNV (unsigned-int void* unsigned-int unsigned-int float unsigned-int))
  ;; GLenum glPathGlyphIndexArrayNV(GLuint firstPathName, GLenum fontTarget, const void *fontName, GLbitfield fontStyle, GLuint firstGlyphIndex, GLsizei numGlyphs, GLuint pathParameterTemplate, GLfloat emScale)
  (define-cdecl unsigned-int glPathGlyphIndexArrayNV (unsigned-int unsigned-int void* unsigned-int unsigned-int int unsigned-int float))
  ;; GLenum glPathMemoryGlyphIndexArrayNV(GLuint firstPathName, GLenum fontTarget, GLsizeiptr fontSize, const void *fontData, GLsizei faceIndex, GLuint firstGlyphIndex, GLsizei numGlyphs, GLuint pathParameterTemplate, GLfloat emScale)
  (define-cdecl unsigned-int glPathMemoryGlyphIndexArrayNV (unsigned-int unsigned-int int void* int unsigned-int int unsigned-int float))
  ;; void glProgramPathFragmentInputGenNV(GLuint program, GLint location, GLenum genMode, GLint components, const GLfloat *coeffs)
  (define-cdecl void glProgramPathFragmentInputGenNV (unsigned-int int unsigned-int int void*))
  ;; void glGetProgramResourcefvNV(GLuint program, GLenum programInterface, GLuint index, GLsizei propCount, const GLenum *props, GLsizei count, GLsizei *length, GLfloat *params)
  (define-cdecl void glGetProgramResourcefvNV (unsigned-int unsigned-int unsigned-int int void* int void* void*))
  ;; void glFramebufferSampleLocationsfvNV(GLenum target, GLuint start, GLsizei count, const GLfloat *v)
  (define-cdecl void glFramebufferSampleLocationsfvNV (unsigned-int unsigned-int int void*))
  ;; void glNamedFramebufferSampleLocationsfvNV(GLuint framebuffer, GLuint start, GLsizei count, const GLfloat *v)
  (define-cdecl void glNamedFramebufferSampleLocationsfvNV (unsigned-int unsigned-int int void*))
  ;; void glResolveDepthValuesNV(void)
  (define-cdecl void glResolveDepthValuesNV ())
  ;; void glScissorExclusiveNV(GLint x, GLint y, GLsizei width, GLsizei height)
  (define-cdecl void glScissorExclusiveNV (int int int int))
  ;; void glScissorExclusiveArrayvNV(GLuint first, GLsizei count, const GLint *v)
  (define-cdecl void glScissorExclusiveArrayvNV (unsigned-int int void*))
  ;; void glMakeBufferResidentNV(GLenum target, GLenum access)
  (define-cdecl void glMakeBufferResidentNV (unsigned-int unsigned-int))
  ;; void glMakeBufferNonResidentNV(GLenum target)
  (define-cdecl void glMakeBufferNonResidentNV (unsigned-int))
  ;; GLboolean glIsBufferResidentNV(GLenum target)
  (define-cdecl uint8_t glIsBufferResidentNV (unsigned-int))
  ;; void glMakeNamedBufferResidentNV(GLuint buffer, GLenum access)
  (define-cdecl void glMakeNamedBufferResidentNV (unsigned-int unsigned-int))
  ;; void glMakeNamedBufferNonResidentNV(GLuint buffer)
  (define-cdecl void glMakeNamedBufferNonResidentNV (unsigned-int))
  ;; GLboolean glIsNamedBufferResidentNV(GLuint buffer)
  (define-cdecl uint8_t glIsNamedBufferResidentNV (unsigned-int))
  ;; void glGetBufferParameterui64vNV(GLenum target, GLenum pname, GLuint64EXT *params)
  (define-cdecl void glGetBufferParameterui64vNV (unsigned-int unsigned-int void*))
  ;; void glGetNamedBufferParameterui64vNV(GLuint buffer, GLenum pname, GLuint64EXT *params)
  (define-cdecl void glGetNamedBufferParameterui64vNV (unsigned-int unsigned-int void*))
  ;; void glGetIntegerui64vNV(GLenum value, GLuint64EXT *result)
  (define-cdecl void glGetIntegerui64vNV (unsigned-int void*))
  ;; void glUniformui64NV(GLint location, GLuint64EXT value)
  (define-cdecl void glUniformui64NV (int unsigned-int))
  ;; void glUniformui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glUniformui64vNV (int int void*))
  ;; void glGetUniformui64vNV(GLuint program, GLint location, GLuint64EXT *params)
  (define-cdecl void glGetUniformui64vNV (unsigned-int int void*))
  ;; void glProgramUniformui64NV(GLuint program, GLint location, GLuint64EXT value)
  (define-cdecl void glProgramUniformui64NV (unsigned-int int unsigned-int))
  ;; void glProgramUniformui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)
  (define-cdecl void glProgramUniformui64vNV (unsigned-int int int void*))
  ;; void glBindShadingRateImageNV(GLuint texture)
  (define-cdecl void glBindShadingRateImageNV (unsigned-int))
  ;; void glGetShadingRateImagePaletteNV(GLuint viewport, GLuint entry, GLenum *rate)
  (define-cdecl void glGetShadingRateImagePaletteNV (unsigned-int unsigned-int void*))
  ;; void glGetShadingRateSampleLocationivNV(GLenum rate, GLuint samples, GLuint index, GLint *location)
  (define-cdecl void glGetShadingRateSampleLocationivNV (unsigned-int unsigned-int unsigned-int void*))
  ;; void glShadingRateImageBarrierNV(GLboolean synchronize)
  (define-cdecl void glShadingRateImageBarrierNV (uint8_t))
  ;; void glShadingRateImagePaletteNV(GLuint viewport, GLuint first, GLsizei count, const GLenum *rates)
  (define-cdecl void glShadingRateImagePaletteNV (unsigned-int unsigned-int int void*))
  ;; void glShadingRateSampleOrderNV(GLenum order)
  (define-cdecl void glShadingRateSampleOrderNV (unsigned-int))
  ;; void glShadingRateSampleOrderCustomNV(GLenum rate, GLuint samples, const GLint *locations)
  (define-cdecl void glShadingRateSampleOrderCustomNV (unsigned-int unsigned-int void*))
  ;; void glTextureBarrierNV(void)
  (define-cdecl void glTextureBarrierNV ())
  ;; void glVertexAttribL1i64NV(GLuint index, GLint64EXT x)
  (define-cdecl void glVertexAttribL1i64NV (unsigned-int int))
  ;; void glVertexAttribL2i64NV(GLuint index, GLint64EXT x, GLint64EXT y)
  (define-cdecl void glVertexAttribL2i64NV (unsigned-int int int))
  ;; void glVertexAttribL3i64NV(GLuint index, GLint64EXT x, GLint64EXT y, GLint64EXT z)
  (define-cdecl void glVertexAttribL3i64NV (unsigned-int int int int))
  ;; void glVertexAttribL4i64NV(GLuint index, GLint64EXT x, GLint64EXT y, GLint64EXT z, GLint64EXT w)
  (define-cdecl void glVertexAttribL4i64NV (unsigned-int int int int int))
  ;; void glVertexAttribL1i64vNV(GLuint index, const GLint64EXT *v)
  (define-cdecl void glVertexAttribL1i64vNV (unsigned-int void*))
  ;; void glVertexAttribL2i64vNV(GLuint index, const GLint64EXT *v)
  (define-cdecl void glVertexAttribL2i64vNV (unsigned-int void*))
  ;; void glVertexAttribL3i64vNV(GLuint index, const GLint64EXT *v)
  (define-cdecl void glVertexAttribL3i64vNV (unsigned-int void*))
  ;; void glVertexAttribL4i64vNV(GLuint index, const GLint64EXT *v)
  (define-cdecl void glVertexAttribL4i64vNV (unsigned-int void*))
  ;; void glVertexAttribL1ui64NV(GLuint index, GLuint64EXT x)
  (define-cdecl void glVertexAttribL1ui64NV (unsigned-int unsigned-int))
  ;; void glVertexAttribL2ui64NV(GLuint index, GLuint64EXT x, GLuint64EXT y)
  (define-cdecl void glVertexAttribL2ui64NV (unsigned-int unsigned-int unsigned-int))
  ;; void glVertexAttribL3ui64NV(GLuint index, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z)
  (define-cdecl void glVertexAttribL3ui64NV (unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glVertexAttribL4ui64NV(GLuint index, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z, GLuint64EXT w)
  (define-cdecl void glVertexAttribL4ui64NV (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glVertexAttribL1ui64vNV(GLuint index, const GLuint64EXT *v)
  (define-cdecl void glVertexAttribL1ui64vNV (unsigned-int void*))
  ;; void glVertexAttribL2ui64vNV(GLuint index, const GLuint64EXT *v)
  (define-cdecl void glVertexAttribL2ui64vNV (unsigned-int void*))
  ;; void glVertexAttribL3ui64vNV(GLuint index, const GLuint64EXT *v)
  (define-cdecl void glVertexAttribL3ui64vNV (unsigned-int void*))
  ;; void glVertexAttribL4ui64vNV(GLuint index, const GLuint64EXT *v)
  (define-cdecl void glVertexAttribL4ui64vNV (unsigned-int void*))
  ;; void glGetVertexAttribLi64vNV(GLuint index, GLenum pname, GLint64EXT *params)
  (define-cdecl void glGetVertexAttribLi64vNV (unsigned-int unsigned-int void*))
  ;; void glGetVertexAttribLui64vNV(GLuint index, GLenum pname, GLuint64EXT *params)
  (define-cdecl void glGetVertexAttribLui64vNV (unsigned-int unsigned-int void*))
  ;; void glVertexAttribLFormatNV(GLuint index, GLint size, GLenum type, GLsizei stride)
  (define-cdecl void glVertexAttribLFormatNV (unsigned-int int unsigned-int int))
  ;; void glBufferAddressRangeNV(GLenum pname, GLuint index, GLuint64EXT address, GLsizeiptr length)
  (define-cdecl void glBufferAddressRangeNV (unsigned-int unsigned-int unsigned-int int))
  ;; void glVertexFormatNV(GLint size, GLenum type, GLsizei stride)
  (define-cdecl void glVertexFormatNV (int unsigned-int int))
  ;; void glNormalFormatNV(GLenum type, GLsizei stride)
  (define-cdecl void glNormalFormatNV (unsigned-int int))
  ;; void glColorFormatNV(GLint size, GLenum type, GLsizei stride)
  (define-cdecl void glColorFormatNV (int unsigned-int int))
  ;; void glIndexFormatNV(GLenum type, GLsizei stride)
  (define-cdecl void glIndexFormatNV (unsigned-int int))
  ;; void glTexCoordFormatNV(GLint size, GLenum type, GLsizei stride)
  (define-cdecl void glTexCoordFormatNV (int unsigned-int int))
  ;; void glEdgeFlagFormatNV(GLsizei stride)
  (define-cdecl void glEdgeFlagFormatNV (int))
  ;; void glSecondaryColorFormatNV(GLint size, GLenum type, GLsizei stride)
  (define-cdecl void glSecondaryColorFormatNV (int unsigned-int int))
  ;; void glFogCoordFormatNV(GLenum type, GLsizei stride)
  (define-cdecl void glFogCoordFormatNV (unsigned-int int))
  ;; void glVertexAttribFormatNV(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride)
  (define-cdecl void glVertexAttribFormatNV (unsigned-int int unsigned-int uint8_t int))
  ;; void glVertexAttribIFormatNV(GLuint index, GLint size, GLenum type, GLsizei stride)
  (define-cdecl void glVertexAttribIFormatNV (unsigned-int int unsigned-int int))
  ;; void glViewportSwizzleNV(GLuint index, GLenum swizzlex, GLenum swizzley, GLenum swizzlez, GLenum swizzlew)
  (define-cdecl void glViewportSwizzleNV (unsigned-int unsigned-int unsigned-int unsigned-int unsigned-int))
  ;; void glFramebufferTextureMultiviewOVR(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint baseViewIndex, GLsizei numViews)
  (define-cdecl void glFramebufferTextureMultiviewOVR (unsigned-int unsigned-int unsigned-int int int int))
) ;[end]
