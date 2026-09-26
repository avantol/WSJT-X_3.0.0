#include "HelpTextWindow.hpp"

#include <QApplication>
#include <QString>
#include <QPalette>

#include "qt_helpers.hpp"

#include "moc_HelpTextWindow.cpp"
#include "revision_utils.hpp"

HelpTextWindow::HelpTextWindow (QString const& title, QString const& text, QFont const& font, QWidget * parent)
  : QLabel {parent, Qt::WindowCloseButtonHint | Qt::WindowMinimizeButtonHint}
{
  setWindowTitle(program_name () + " - " + title);
  setMargin (10);
  setBackgroundRole (QPalette::Base);
  setAutoFillBackground (true);
  setStyleSheet (font_as_stylesheet (font));
  setText (text);
  setMinimumSize (sizeHint ());
}
