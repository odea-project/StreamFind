// nts_blank_subtraction.h
// Feature blank subtraction for NTS_DATA

#ifndef NTS_BLANK_SUBTRACTION_H
#define NTS_BLANK_SUBTRACTION_H

#include <vector>
#include <string>
#include "../streamcraft/streamcraft.h"

namespace nts
{
  struct NTS_DATA;

  namespace blank_subtraction
  {
    void subtract_blank_impl(
        NTS_DATA &nts_data,
        float blankThreshold,
        float rtExpand,
        float mzExpand,
        float minTracesIntensity = 0.0f);
  } // namespace blank_subtraction
} // namespace nts

#endif // NTS_BLANK_SUBTRACTION_H
