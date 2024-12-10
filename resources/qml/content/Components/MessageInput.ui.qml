import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import Components 1.0
import Futr 1.0

Pane {
    id: root
    
    property string placeholderText: qsTr("Type a message...")
    property string buttonText: qsTr("Send")
    
    signal messageSent(string text)
    
    Layout.fillWidth: true
    Layout.leftMargin: Constants.spacing_m
    Layout.rightMargin: Constants.spacing_m
    Layout.bottomMargin: Constants.spacing_m
    padding: Constants.spacing_m
    
    background: Rectangle {
        color: Material.dialogColor
        radius: 5
        border.color: Material.dividerColor
        border.width: 1
    }

    RowLayout {
        width: parent.width
        spacing: Constants.spacing_m

        ProfilePicture {
            imageSource: Util.getProfilePicture(mypicture, mynpub)
            Layout.preferredWidth: 36
            Layout.preferredHeight: 36
            Layout.alignment: Qt.AlignVCenter
        }

        TextArea {
            id: messageInput
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignVCenter
            placeholderText: root.placeholderText
            wrapMode: TextArea.Wrap
            font: Constants.fontMedium
            focus: true
            
            Keys.onReturnPressed: function(event) {
                if (event.modifiers & Qt.ControlModifier) {
                    event.accepted = true
                    sendMessage()
                } else {
                    event.accepted = false
                }
            }
        }

        Button {
            text: root.buttonText
            highlighted: true
            enabled: messageInput.text.trim().length > 0
            implicitWidth: 80
            implicitHeight: 36
            Layout.alignment: Qt.AlignVCenter
            
            onClicked: sendMessage()
        }
    }

    function sendMessage() {
        if (messageInput.text.trim() !== "") {
            root.messageSent(messageInput.text)
            messageInput.text = ""
        }
    }

    function clear() {
        messageInput.text = ""
    }
} 