#ifdef __cplusplus
extern "C"
#endif
void mynvtxstart_(const char *name);
#ifdef __cplusplus
extern "C"
#endif
void nvtxRangePop();
#define PUSH_RANGE(name) { mynvtxstart_(name);}
#define POP_RANGE(name) nvtxRangePop();
