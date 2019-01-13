//
// Created by Christoffer Lerno on 2019-01-12.
// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
//
#include "target_info.h"
#include "error.h"
#include <string.h>
#include <sys/utsname.h>
#include <errno.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))

typedef struct _ArchList
{
    Arch arch;
    const char* name;
} ArchList;

static ArchList archList[] = {
    { ARCH_UNKNOWN,   "unknown" },
    { ARCH_I686,      "i686" },
    { ARCH_X86_64,    "x86_64" },
    { ARCH_ARM,       "arm" },
};

const char *arch_name(Arch arch)
{
    for (unsigned i = 0; i < ARRAY_SIZE(archList); i++)
    {
        const ArchList *a = &archList[i];
        if (a->arch == arch) return a->name;
    }
    return "<missing>";
}

static Arch arch_from_string(const char *name)
{
    if (!name) return ARCH_UNKNOWN;
    for (unsigned i = 0; i < ARRAY_SIZE(archList); i++)
    {
        const ArchList *a = &archList[i];
        if (strcmp(a->name, name) == 0) return a->arch;
    }
    return ARCH_UNKNOWN;
}

static Arch valid_arch_from_string(const char *name)
{
    Arch arch = arch_from_string(name);
    if (arch == ARCH_UNKNOWN)
    {
        PRINT_ERROR("unsupported arch: %s", name);
        exit(EXIT_FAILURE);
    }
    return arch;
}

typedef struct {
    Vendor vendor;
    const char* name;
} VendorList;

static VendorList vendorList[] = {
        { VENDOR_UNKNOWN, "unknown" },
        { VENDOR_APPLE, "apple" },
};

const char *vendor_name(Vendor vendor)
{
    for (unsigned i = 0; i < ARRAY_SIZE(vendorList); i++)
    {
        const VendorList *v = &vendorList[i];
        if (v->vendor == vendor) return v->name;
    }
    return "<missing>";
}

static Vendor vendor_from_string(const char *name)
{
    if (!name) VENDOR_UNKNOWN;
    for (unsigned i = 0; i < ARRAY_SIZE(vendorList); i++)
    {
        const VendorList *v = &vendorList[i];
        if (strcmp(v->name, name) == 0) return v->vendor;
    }
    return VENDOR_UNKNOWN;
}

typedef struct
{
    System sys;
    const char *name;
} SystemList;

static SystemList sysList[] = {
        { SYS_UNKNOWN, "unknown" },
        { SYS_LINUX, "linux" },
        { SYS_DARWIN, "darwin" },
        { SYS_CYGWIN, "cygwin" },
};


const char *system_name(System sys)
{
    for (unsigned i = 0; i < ARRAY_SIZE(sysList); i++)
    {
        const SystemList *s = &sysList[i];
        if (s->sys == sys) return s->name;
    }
    return "";
}

System system_from_string(const char *name)
{
    for (unsigned i = 0; i < ARRAY_SIZE(sysList); i++)
    {
        const SystemList *s = &sysList[i];
        if (strcasecmp(s->name, name) == 0) return s->sys;
    }
    return SYS_UNKNOWN;
}

typedef struct
{
    Abi abi;
    const char *name;
} AbiList;

static AbiList abiList[] = {
        { ABI_UNKNOWN, "unknown" },
        { ABI_GNU, "gnu" },
        { ABI_GNUEABI, "gnueabi" },
        { ABI_MACHO, "macho" },
        { ABI_WIN32, "win32" },
};

const char *abi_name(Abi abi)
{
    for (unsigned i = 0; i < ARRAY_SIZE(abiList); i++)
    {
        const AbiList *a = &abiList[i];
        if (a->abi == abi) return a->name;
    }
    return "<missing>";
}

static Abi abi_from_string(const char *name)
{
    for (unsigned i = 0; i < ARRAY_SIZE(abiList); i++)
    {
        const AbiList *a = &abiList[i];
        if (strcmp(a->name, name) == 0) return a->abi;
    }
    return ABI_UNKNOWN;
}


void target_info_from_native(TargetInfo *info)
{
    struct utsname un;
    if (uname(&un) != 0)
    {
        PRINT_ERROR("error getting system info: %s", strerror(errno));
        exit(EXIT_FAILURE);
    }

    info->sys = system_from_string(un.sysname);
    switch (info->sys)
    {
        case SYS_UNKNOWN:
            PRINT_ERROR("unsupported system: %s\n", un.sysname);
            exit(EXIT_FAILURE);
        case SYS_LINUX:
            info->vendor = VENDOR_UNKNOWN;   // hardcoded
            info->abi = ABI_GNU;             // hardcoded
            break;
        case SYS_DARWIN:
            info->vendor = VENDOR_APPLE;     // hardcoded
            info->abi = ABI_MACHO;           // hardcoded
            break;
        case SYS_CYGWIN:
            info->vendor = VENDOR_UNKNOWN;
            info->abi = ABI_WIN32;
            break;
    }


    info->arch = valid_arch_from_string(un.machine);
}

bool target_info_from_string(TargetInfo *info, const char *triple)
{
    // expect format: <arch><sub>-<vendor>-<sys>-<abi>
    size_t len = strlen(triple);
    ASSERT(len < 255, "Triple too long!");
    char buf[256];
    strncpy(buf, 256, triple);
    const char s[2] = "-";
    char *arch = strtok(buf, s);
    char *vendor = arch ? strtok(NULL, s) : NULL;
    char *sys = vendor ? strtok(NULL, s) : NULL;
    char *abi = vendor ? strtok(NULL, s) : NULL;
    if (!arch || !vendor || !sys || !abi || strtok(NULL, s))
    {
        PRINT_ERROR("Malformed triple %s", triple);
        return false;
    }
    // support ARM sub
    info->arch = valid_arch_from_string(arch);

    info->vendor = vendor_from_string(arch);
    info->sys = system_from_string(sys);
    if (info->sys == SYS_UNKNOWN)
    {
        PRINT_ERROR("unsupported system: %s", sys);
        return false;
    }
    info->abi = abi_from_string(abi);
    if (info->abi == ABI_UNKNOWN)
    {
        PRINT_ERROR("unsupported ABI: %s", abi);
        return false;
    }
    return true;
}

const char *target_info_to_string(TargetInfo *info)
{
    static char result[80];
    sprintf(result, "%s-%s-%s-%s",
        arch_name(info->arch), vendor_name(info->vendor), system_name(info->sys), abi_name(info->abi));
    return result;
}

static Token remove_gcc_register_prefix(Token name)
{
    if (name.start[0] == '%' || name.start[0] == '#')
    {
        name.start++;
        name.length--;
    }
    return name;
}

bool target_info_is_valid_gcc_register_name(TargetInfo *info, Token *name)
{
    if (name->length == 0) return false;
    Token register_name = remove_gcc_register_prefix(*name);

#if 0
    TODO
  llvm::ArrayRef<const char *> Names = getGCCRegNames();

  // If we have a number it maps to an entry in the register name array.
  if (isdigit(Name[0])) {
    unsigned n;
    if (!Name.getAsInteger(0, n))
      return n < Names.size();
  }

  // Check register names.
  if (std::find(Names.begin(), Names.end(), Name) != Names.end())
    return true;

  // Check any additional names that we have.
  for (const AddlRegName &ARN : getGCCAddlRegNames())
    for (const char *AN : ARN.Names) {
      if (!AN)
        break;
      // Make sure the register that the additional name is for is within
      // the bounds of the register names from above.
      if (AN == Name && ARN.RegNum < Names.size())
        return true;
    }

  // Now check aliases.
  for (const GCCRegAlias &GRA : getGCCRegAliases())
    for (const char *A : GRA.Aliases) {
      if (!A)
        break;
      if (A == Name)
        return true;
    }
#endif
    return true;
}

bool target_info_is_valid_clobber(TargetInfo *target_info, Token *name)
{
    return target_info_is_valid_gcc_register_name(target_info, name)
            || token_compare_str(name, "memory") || token_compare_str(name, "cc");
}

bool target_info_validate_input_constraint(TargetInfo *targetInfo, ConstraintInfo *info)
{
    return true;
}

bool target_info_validate_output_constraint(TargetInfo *info, ConstraintInfo *constraint_info)
{
  const char *name = constraint_info->constraint_str;
  // An output constraint must start with '=' or '+'
  switch (name[0])
  {
      case '=':
          break;
      case '+':
          constraint_info->read_write = true;
          // TODO;
          break;
      default:
          return false;
  }

    // TODO see Clang:: TargetInfo:validateOutputConstraint()
    return true;
}

/*
 *     bool hasTiedOperand() const { return TiedOperand != -1; }
     unsigned getTiedOperand() const {
       assert(hasTiedOperand() && "Has no tied operand!");
       return (unsigned)TiedOperand;
     }

 		void setRequiresImmediate(int Min, int Max) {
		  Flags |= CI_ImmediateConstant;
		  ImmRange.Min = Min;
		  ImmRange.Max = Max;
		}
#if 0
		void setRequiresImmediate(llvm::ArrayRef<int> Exacts) {
		  Flags |= CI_ImmediateConstant;
		  for (int Exact : Exacts)
			ImmSet.insert(Exact);
		}
#endif
		void setRequiresImmediate(int Exact) {
		  Flags |= CI_ImmediateConstant;
		  ImmSet.insert(Exact);
		}
		void setRequiresImmediate() {
		  Flags |= CI_ImmediateConstant;
		  ImmRange.Min = INT_MIN;
		  ImmRange.Max = INT_MAX;
		}

		/// \brief Indicate that this is an input operand that is tied to
		/// the specified output operand.
		///
		/// Copy over the various constraint information from the output.
		void setTiedOperand(unsigned N, ConstraintInfo &Output) {
		  Output.setHasMatchingInput();
		  Flags = Output.Flags;
		  TiedOperand = N;
		  // Don't copy Name or constraint string.
		}

 	public:
	  ConstraintInfo(llvm::StringRef ConstraintStr, llvm::StringRef Name)
        : Flags(0), TiedOperand(-1), ConstraintStr(ConstraintStr.str()),
          Name(Name.str()) {
      ImmRange.Min = ImmRange.Max = 0;
    }

 */
