import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Dialogs 1.0
import HsQML.Model 1.0
import Futr 1.0
import Profile 1.0

Item {
    id: homeScreen
    width: parent.width
    height: parent.height

    Component.onCompleted: {
        setCurrentProfile(mynpub)
        profileLoader.setSource(
            "Profile/Profile.ui.qml",
            { 
                "profileData": currentProfile,
                "npub": mynpub 
            }
        )
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 10

        TopBar {
            Layout.fillWidth: true
            height: 80
        }

        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true

            Row {
                anchors.fill: parent
                anchors.leftMargin: 10
                anchors.rightMargin: 10
                spacing: 10

                FollowList {}

                Rectangle {
                    width: parent.width * 0.4 - (parent.spacing * 2 / 3)
                    height: parent.height
                    color: Material.backgroundColor

                    Loader {
                        id: chatLoader
                        anchors.fill: parent
                    }
                }

                Rectangle {
                    width: parent.width * 0.3 - (parent.spacing * 2 / 3)
                    height: parent.height
                    color: Material.backgroundColor

                    Loader {
                        id: profileLoader
                        anchors.fill: parent
                        anchors.rightMargin: 10
                        anchors.bottomMargin: 5
                    }
                }
            }
        }
    }

    RelayMgmtDialog {
        id: relayMgmtDialog
    }

    ShowKeysDialog {
        id: showKeysDialog
    }
}
