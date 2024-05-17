#include <nvToolsExt.h>
#include <string.h>

// mpicc -c nvtx.c -O2

   uint32_t myadler32(const unsigned char *data)
   {
      const uint32_t MOD_ADLER = 65521;
      uint32_t a = 1, b = 0;
      size_t index;
      for (index = 0; data[index] != 0; ++index)
      {
         a = (a + data[index]*2) % MOD_ADLER;
         b = (b + a) % MOD_ADLER;
      }
      return (b << 16) | a;
   }
   extern int map_start(const char * str);
#ifdef NVTX_VERYVERBOSE
   const char namestack[256][256];
   int istack=0;
#endif
//   int first = 1;
void mynvtxstart_(const char *name) {
   if (!map_start(name)) {
#ifdef NVTX_VERYVERBOSE
      for(int i=0;i<istack;i++) printf(" ");
      printf("Skipped open --- %s\n",name);
#endif
      return;
   }

   int hash = 0;
   int color_id = myadler32((const unsigned char*)name);
   int r,g,b;
   r=color_id & 0x000000ff;
   g=(color_id & 0x000ff000) >> 12;
   b=(color_id & 0x0ff00000) >> 20;
   if (r<64 & g<64 & b<64) {
      r=r*3;
      g=g*3+64;
      b=b*4;
   }

   color_id = 0xff000000 | (r << 16) | (g << 8) | (b);
   nvtxEventAttributes_t eventAttrib = {0};
   eventAttrib.version = NVTX_VERSION;
   eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
   eventAttrib.colorType = NVTX_COLOR_ARGB;
   eventAttrib.color = color_id;
   eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
   eventAttrib.message.ascii = name;
#ifdef NVTX_VERYVERBOSE
   for(int i=0;i<istack;i++) printf(" ");
   printf("Opening %s\n",name);
#endif
   nvtxRangePushEx(&eventAttrib);
#ifdef NVTX_VERYVERBOSE
   strncpy(namestack[istack], name, 128);
   istack++;
#endif

}
#ifdef NVTX_VERYVERBOSE
void mynvtxend_(const char *name) {
#else
void mynvtxend_() {
#endif
   if (!map_stop()){
#ifdef NVTX_VERYVERBOSE
      for(int i=0;i<istack-1;i++) printf(" ");
      printf("Skipped end --- %s\n",name);
#endif
      return;
   }
#ifdef NVTX_VERYVERBOSE
   istack--;
   if (istack < 0) {
      printf("NVTX error negative stack\n");
      exit(-1);
   }
   for(int i=0;i<istack;i++) printf(" ");
   printf("Closing %s\n",name);
   if (strcmp(name,namestack[istack])) {
      printf("Error just closed the wrong marker: %s expected: %s\n",name, namestack[istack]);
      abort(1);
   }
#endif
   nvtxRangePop();
}
