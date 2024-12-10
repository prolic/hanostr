import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtGraphicalEffects 1.15

Item {
    id: root
    width: 24
    height: 24

    property string connectionState: "Disconnected"
    property int connectionRetries: 0

    Image {
        id: connectionStateIcon
        anchors.fill: parent
        source: {
            switch(root.connectionState) {
                case "Connected":
                    return "qrc:/icons/wifi.svg"
                case "Connecting":
                    return "qrc:/icons/sync.svg"
                case "Disconnected":
                default:
                    return "qrc:/icons/wifi_off.svg"
            }
        }
    }

    ColorOverlay {
        anchors.fill: parent
        source: connectionStateIcon
        color: {
            switch(root.connectionState) {
                case "Connected":
                    return "green"
                case "Connecting":
                    return "orange"
                case "Disconnected":
                default:
                    return "red"
            }
        }
    }

    Text {
        id: retryCountText
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        font.pixelSize: 10
        color: Material.foreground
        text: root.connectionRetries
        visible: root.connectionState === "Connecting" && root.connectionRetries > 0

        Rectangle {
            anchors.fill: parent
            anchors.margins: -2
            color: "red"
            radius: width / 2
            z: -1
        }
    }

    ToolTip.visible: connectionStateMouseArea.containsMouse
    ToolTip.text: {
        switch(root.connectionState) {
            case "Connected":
                return qsTr("Connected")
            case "Connecting":
                return root.connectionRetries > 0 ? 
                    qsTr("Connecting... (Retry %1)").arg(root.connectionRetries) :
                    qsTr("Connecting...")
            case "Disconnected":
            default:
                return qsTr("Disconnected")
        }
    }

    MouseArea {
        id: connectionStateMouseArea
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
    }
}
