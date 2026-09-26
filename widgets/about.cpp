#include "about.h"

#include <QCoreApplication>
#include <QString>

#include "revision_utils.hpp"

#include "ui_about.h"

CAboutDlg::CAboutDlg(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::CAboutDlg)
{
  ui->setupUi(this);

  //avt 9/25/26 "FT8 (UDP Edition)" branding
  ui->labelTxt->setText ("<h2>" + QString {"FT8 (UDP Edition) v3-"
                                             + revision () + " (" + testVer() + ")<br />(mod by WM8Q, qrz.com/db/WM8Q)"}.simplified () + "</h2>"
    "<h2><a href=\"https://github.com/avantol/FT8-UDP-Edition\">source code for modifications</a></h2>"
    "FT8 (UDP Edition) is based on WSJT-X, which implements a number <br />"
    "of digital modes designed for weak-signal Amateur Radio <br />"
    "communication.  <br /><br />"
    "&copy; 2001-2025 by Joe Taylor, K1JT, Bill Somerville, G4WJS, <br />"
    "Steve Franke, K9AN, Nico Palermo, IV3NWV, <br />"
    "Uwe Risse, DG2YCB, Brian Moran, N9ADG, <br />"
    "and Roger Rehr, W3SZ.<br /><br />"
    "We gratefully acknowledge contributions from AC6SL, AE4JY,<br />"
    "DF2ET, DJ0OT, DL3WDG, EA4AC, G4KLA, IW3RAB, JA7UDE,<br />"
    "K3WYC, KA1GT, KA6MAL, KA9Q, KB1ZMX, KD6EKQ, KG4IYS, KI7MT,<br />"
    "KK1D, ND0B, PY1ZRJ, PY2SDR, VE1SKY, VK3ACF, VK4BDJ,<br />"
    "VK7MO, VR2UPU, W3DJS, W4TI, W4TV, and W9MDB.<br /><br />"
    "FT8 (UDP Edition), like WSJT-X, is licensed under the terms <br />"
    "of Version 3 of the GNU General Public License (GPL) <br /><br />"
    "<a href=\"https://www.gnu.org/licenses/gpl-3.0.txt\">"
    "<img src=\":/gpl-v3-logo.svg\" height=\"80\" /><br />"
    "https://www.gnu.org/licenses/gpl-3.0.txt</a>");
}

CAboutDlg::~CAboutDlg()
{
}
