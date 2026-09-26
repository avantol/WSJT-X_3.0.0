#include "revision_utils.hpp"

#include <cstring>

#include <QCoreApplication>
#include <QRegularExpression>

#include "scs_version.h"

namespace
{
  QString revision_extract_number (QString const& s)
  {
    QString revision;

    // try and match a number (hexadecimal allowed)
    QRegularExpression re {R"(^[$:]\w+: (r?[\da-f]+[^$]*)\$$)"};
    auto match = re.match (s);
    if (match.hasMatch ())
      {
        revision = match.captured (1);
      }
    return revision;
  }
}

QString testVer ()    //avt 1/29/26
{
  return "5";
}

QString revision (QString const& scs_rev_string)
{
  return "105";
  QString result;
  auto revision_from_scs = revision_extract_number (scs_rev_string);

#if defined (CMAKE_BUILD)
  QString scs_info {":Rev: " SCS_VERSION_STR " $"};

  auto revision_from_scs_info = revision_extract_number (scs_info);
  if (!revision_from_scs_info.isEmpty ())
    {
      // we managed to get the revision number from svn info etc.
      result = revision_from_scs_info;
    }
  else if (!revision_from_scs.isEmpty ())
    {
      // fall back to revision passed in if any
      result = revision_from_scs;
    }
  else
    {
      // match anything
      QRegularExpression re {R"(^[$:]\w+: ([^$]*)\$$)"};
      auto match = re.match (scs_info);
      if (match.hasMatch ())
        {
          result = match.captured (1);
        }
    }
#else
  if (!revision_from_scs.isEmpty ())
    {
      // not CMake build so all we have is revision passed
      result = revision_from_scs;
    }
#endif
  return result.trimmed ();
}

QString version (bool include_patch)
{
#if defined (CMAKE_BUILD)
  QString v {TO_STRING__ (PROJECT_VERSION_MAJOR) "." TO_STRING__ (PROJECT_VERSION_MINOR)};
  if (include_patch)
    {
      v += "." TO_STRING__ (PROJECT_VERSION_PATCH) + QString {BUILD_TYPE_REVISION};
    }
#else
  QString v {"Not for Release"};
#endif
  return v;
}

// avt 9/25/26 "FT8 (UDP Edition)" branding. The application name is
// still "WSJT-X", so settings, the writeable data directory and the UDP
// id are unchanged; only what the user sees is re-branded. MAP65 and
// QMAP set their own application names and keep their own titles.
// The displayed program name. Use this instead of
// QCoreApplication::applicationName () anywhere the name is shown to
// the user, such as window titles; applicationName () itself must stay
// "WSJT-X" because it locates the settings and data directory, keys the
// jt9 shared memory and identifies us to UDP clients.
QString program_name ()
{
  auto const& app_name = QCoreApplication::applicationName ();
  if (app_name.startsWith ("WSJT-X"))
    {
      // keep any multi-instance suffix, e.g. " - 7300" or " - test"
      return "FT8 (UDP Edition)" + app_name.mid (6);
    }
  return app_name;                // MAP65, QMAP and the UDP examples
}

QString program_title (QString const& revision)
{
  auto const& app_name = QCoreApplication::applicationName ();
  if (app_name.startsWith ("WSJT-X"))
    {
      return "FT8 (UDP Edition) v3-" + ::revision () + app_name.mid (6);
    }
  QString id {app_name + "   v" + QCoreApplication::applicationVersion ()};
  return id + " " + revision;
}
