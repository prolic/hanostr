import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

Rectangle {
    Layout.preferredWidth: 36
    Layout.preferredHeight: 36
    Layout.alignment: Qt.AlignVCenter
    Layout.topMargin: 0
    radius: width / 2
    color: Material.dividerColor
    clip: true

    property string imageSource

    Image {
        anchors.fill: parent
        source: parent.imageSource
        smooth: true
        fillMode: Image.PreserveAspectCrop
    }
}
