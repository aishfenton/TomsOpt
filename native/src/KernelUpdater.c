#include <stdio.h>
#include "tomsopt_kernel_KernelUpdaterNative.h"
#include "tomsopt_kernel_ARDKernel.h"
#include "tomsopt_kernel_Matern52.h"
#include <math.h>
#include <string.h>

// -----------------
// Declarations
// -----------------

typedef double (*kernelFn)(double *, double *, int);

double ardKernel(double *v1, double *v2, int dim);
double matern52(double *v1, double *v2, int dim);

void updateKernel(double *a, int aOffset, int aLength,
                  double *b, int bOffset, int bLength,
                  double *c, int cOffset, int cLength,
                  int dim, double noise, kernelFn kernelPtr);

const double SQRT5 = 2.23606797749979;
const double D53 = 5.0 / 3.0;


// -----------------
// JNI Wrappers
// -----------------

/*
 * Class:     tomsopt_kernel_KernelUpdaterNative
 * Method:    _nativeUpdate
 * Signature: ([DII[DII[DIIIDLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_tomsopt_kernel_KernelUpdaterNative__1nativeUpdate
  (JNIEnv *env, jobject classz,
   jdoubleArray ja, jint aOffset, jint aLength,
   jdoubleArray jb, jint bOffset, jint bLength,
   jdoubleArray jc, jint cOffset, jint cLength,
   jint dim, jdouble noise, jstring jKernelStr) {

  jdouble *a = (*env)->GetPrimitiveArrayCritical(env, ja, 0);
  jdouble *b = (*env)->GetPrimitiveArrayCritical(env, jb, 0);
  jdouble *c = (*env)->GetPrimitiveArrayCritical(env, jc, 0);
  const char *kernelStr = (*env)->GetStringUTFChars(env, jKernelStr, NULL);
  kernelFn kernelPtr;

  if (strcmp(kernelStr, "tomsopt.kernel.ARDKernel")==0) {
    kernelPtr = &ardKernel;
  }
  else if (strcmp(kernelStr, "tomsopt.kernel.Matern52")==0) {
    kernelPtr = &matern52;
  }
  else  {
    fprintf(stderr, "ERROR: Couldn't find kernel %s\n", kernelStr);
    return 1;
  }

  updateKernel(a, aOffset, aLength,
               b, bOffset, bLength,
               c, cOffset, cLength,
               dim, noise, kernelPtr);

  (*env)->ReleasePrimitiveArrayCritical(env, ja, a, 0);
  (*env)->ReleasePrimitiveArrayCritical(env, jb, b, 0);
  (*env)->ReleasePrimitiveArrayCritical(env, jc, c, 0);
  (*env)->ReleaseStringUTFChars(env, jKernelStr, kernelStr);

  return 0;
}

/*
 * Class:     tomsopt_kernel_ARDKernel
 * Method:    applyNative
 * Signature: ([DI[DII)D
 */
JNIEXPORT jdouble JNICALL Java_tomsopt_kernel_ARDKernel_applyNative
  (JNIEnv *env, jobject classz, jdoubleArray jx1, jint jx1Offset, jdoubleArray jx2, jint jx2Offset, jint dim) {

  jdouble *x1 = (*env)->GetPrimitiveArrayCritical(env, jx1, 0);
  jdouble *x2 = (*env)->GetPrimitiveArrayCritical(env, jx2, 0);

  jdouble result = ardKernel(&x1[jx1Offset], &x2[jx2Offset], dim);

  (*env)->ReleasePrimitiveArrayCritical(env, jx1, x1, 0);
  (*env)->ReleasePrimitiveArrayCritical(env, jx2, x2, 0);

  return result;
}

/*
 * Class:     tomsopt_kernel_Matern52
 * Method:    applyNative
 * Signature: ([DI[DII)D
 */
JNIEXPORT jdouble JNICALL Java_tomsopt_kernel_Matern52_applyNative
  (JNIEnv *env, jobject classz, jdoubleArray jx1, jint jx1Offset, jdoubleArray jx2, jint jx2Offset, jint dim) {

  jdouble *x1 = (*env)->GetPrimitiveArrayCritical(env, jx1, 0);
  jdouble *x2 = (*env)->GetPrimitiveArrayCritical(env, jx2, 0);

  jdouble result = matern52(&x1[jx1Offset], &x2[jx2Offset], dim);

  (*env)->ReleasePrimitiveArrayCritical(env, jx1, x1, 0);
  (*env)->ReleasePrimitiveArrayCritical(env, jx2, x2, 0);

  return result;
}

// -----------------
// Implementations
// -----------------

/**
 * TODO:
 * - All Kernels need an extra params array passed to them.
 */
void updateKernel(double *a, int aOffset, int aLength,
                  double *b, int bOffset, int bLength,
                  double *c, int cOffset, int cLength,
                  int dim, double noise, kernelFn kernelPtr) {

  if (cLength != (bLength * aLength / pow(dim, 2))) {
    fprintf(stderr, "ERROR: C array (length: %d) should be long enough to contain number of cols in A * B\n", cLength);
    return;
  }

//  printf("ao%d bo%d co%d al%d bl%d cl%d ad0%f bd0%f bd1%f\n", aOffset, bOffset, cOffset, aLength, bLength, cLength, a[0], b[0], b[7]);
//  fflush(stdout);

  int aCols = aLength / dim;
  int bCols = bLength / dim;

  for (int i = 0 ; i < bCols; i++) {
    int bOff = (i * dim) + bOffset;
    double *bi = &b[bOff];

    for (int j = 0 ; j < aCols; j++) {
      int aOff = (j * dim) + aOffset;
      int cOff = (i * aCols) + j + cOffset;

      c[cOff] = (* kernelPtr)(&a[aOff], bi, dim) + noise;
    }
  }

}

double ardKernel(double *v1, double *v2, int dim) {
  double sdist = 0.0;
  double dot = 0.0;

  for (int i = 0; i < dim ; i++) {
    double a = v1[i];
    double b = v2[i];

    sdist += pow(a-b, 2);
    dot += a * b;
  }
  double result = (0.1 * exp(-0.5 * 0.1 * sdist)) + 0.1 + (0.1 * dot);
  return result;
}

double matern52(double *v1, double *v2, int dim) {
  double r2 = 0.0;
  for (int i = 0; i < dim ; i++) {
    double a = v1[i];
    double b = v2[i];

    r2 += pow(a - b, 2) * 0.1;
  }
  double r = sqrt(r2);
  double result = 0.1 * (1 + (SQRT5 * r) + (D53 * r2)) * exp(-(SQRT5 * r));
  return result;
}


