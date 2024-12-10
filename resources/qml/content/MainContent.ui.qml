import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Controls.Material.impl 2.15
import QtQuick.Layouts 1.15

import Components 1.0
import Dialogs 1.0
import Futr 1.0
import HsQML.Model 1.0

Rectangle {
    id: chat
    color: Material.backgroundColor
    radius: 5
    border.color: Material.dividerColor
    border.width: 1

    property string npub: ""
    property var profileData

    PostDialog {
        id: quoteReplyDialog
        inputPlaceholder: qsTr("Add a quote...")
        buttonText: qsTr("Quote")
        isQuoteMode: true

        onMessageSubmitted: function(text) {
            if (targetPost.postType == "repost") {
                quoteRepost(targetPost.referencedEventId, text)
            } else {
                quoteRepost(targetPost.id, text)
            }
        }
    }

    PostDialog {
        id: commentsDialog
        inputPlaceholder: qsTr("Post your reply")
        buttonText: qsTr("Reply")
        isQuoteMode: false
        
        onMessageSubmitted: function(text) {
            comment(targetPost.id, text)
        }
    }

    Menu {
        id: repostMenu
        property var targetPost: null

        MenuItem {
            text: qsTr("Repost")
            onTriggered: {
                if (repostMenu.targetPost.postType == "repost") {
                    repost(repostMenu.targetPost.referencedEventId, text)
                } else {
                    repost(repostMenu.targetPost.id, text)
                }
            }
        }

        MenuItem {
            text: qsTr("Quote Post")
            onTriggered: {
                quoteReplyDialog.targetPost = repostMenu.targetPost
                quoteReplyDialog.open()
            }
        }
    }

    ColumnLayout {
        id: mainContentArea
        anchors.fill: parent
        anchors.margins: Constants.spacing_xs
        spacing: Constants.spacing_m

        TabBar {
            id: chatTypeSelector
            Layout.fillWidth: true
            Layout.preferredHeight: 48
            Layout.topMargin: Constants.spacing_xs
            contentWidth: width

            TabButton {
                id: publicTab
                implicitWidth: chatTypeSelector.width / 2
                text: qsTr("Short Text Notes")

                background: Rectangle {
                    color: parent.down || parent.checked ? Material.primaryColor : "transparent"
                    opacity: parent.down || parent.checked ? 0.12 : 1.0
                }

                contentItem: RowLayout {
                    spacing: Constants.spacing_s
                    width: parent.width
                    height: parent.height

                    Item { Layout.fillWidth: true }

                    Image {
                        source: "qrc:/icons/public.svg"
                        sourceSize.width: 20
                        sourceSize.height: 20
                        Layout.preferredWidth: 20
                        Layout.preferredHeight: 20
                    }

                    Text {
                        text: parent.parent.text
                        color: Material.foreground
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }

                    Item { Layout.fillWidth: true }
                }
            }

            TabButton {
                id: privateTab
                implicitWidth: chatTypeSelector.width / 2
                text: npub === mynpub ? qsTr("Saved Messages") : qsTr("Private Chat")

                background: Rectangle {
                    color: parent.down || parent.checked ? Material.primaryColor : "transparent"
                    opacity: parent.down || parent.checked ? 0.12 : 1.0
                }

                contentItem: RowLayout {
                    spacing: Constants.spacing_s
                    width: parent.width
                    height: parent.height

                    Item { Layout.fillWidth: true }

                    Image {
                        source: "qrc:/icons/encrypted.svg"
                        sourceSize.width: 20
                        sourceSize.height: 20
                        Layout.preferredWidth: 20
                        Layout.preferredHeight: 20
                    }

                    Text {
                        text: parent.parent.text
                        color: Material.foreground
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }

                    Item { Layout.fillWidth: true }
                }
            }
        }

        // Stack Layout to switch between public/private views
        StackLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.topMargin: Constants.spacing_m
            currentIndex: chatTypeSelector.currentIndex

            // Public Notes View
            Item {
                Layout.fillWidth: true
                Layout.fillHeight: true

                ColumnLayout {
                    anchors.fill: parent
                    spacing: Constants.spacing_m

                    // Public notes list (now takes all space except input area)
                    ListView {
                        id: postsView
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        leftMargin: Constants.spacing_m
                        rightMargin: Constants.spacing_m
                        spacing: Constants.spacing_m
                        bottomMargin: Constants.spacing_m

                        model: AutoListModel {
                            id: postsModel
                            source: posts
                            mode: AutoListModel.ByKey
                            equalityTest: function (oldItem, newItem) {
                                return oldItem.content === newItem.content
                                    && oldItem.timestamp === newItem.timestamp
                            }
                        }

                        delegate: Loader {
                            active: modelData !== undefined && modelData !== null
                            width: postsView.width - postsView.leftMargin - postsView.rightMargin
                            height: active ? item.implicitHeight : 0

                            sourceComponent: PostContent {
                                post: modelData

                                onCommentClicked: {
                                    if (modelData) {    
                                        commentsDialog.targetPost = modelData
                                        commentsDialog.open()
                                    }
                                }

                                onRepostClicked: {
                                    if (modelData) {
                                        repostMenu.targetPost = modelData
                                        repostMenu.popup()
                                    }
                                }
                            }
                        }

                        onCountChanged: {
                            if (atYEnd || count === 1) {
                                positionViewAtEnd()
                            }
                        }

                        Component.onCompleted: {
                            positionViewAtEnd()
                        }

                        ScrollBar.vertical: ScrollBar {
                            id: scrollBar
                            active: true
                            interactive: true
                            policy: ScrollBar.AsNeeded
                            
                            contentItem: Rectangle {
                                implicitWidth: 6
                                radius: width / 2
                                color: scrollBar.pressed ? Material.scrollBarPressedColor :
                                       scrollBar.hovered ? Material.scrollBarHoveredColor :
                                                         Material.scrollBarColor
                                opacity: scrollBar.active ? 1 : 0
                                
                                Behavior on opacity {
                                    NumberAnimation { duration: 150 }
                                }
                            }
                        }

                        onContentHeightChanged: {
                            if (atYEnd) {
                                positionViewAtEnd()
                            }
                        }
                    }

                    // Input area for new public notes (at the bottom)
                    MessageInput {
                        placeholderText: qsTr("What's on your mind?")
                        buttonText: qsTr("Post")
                        onMessageSent: function(text) {
                            sendShortTextNote(text)
                        }
                    }
                }
            }

            // Private Chat View
            ColumnLayout {
                spacing: Constants.spacing_m

                DMRelays {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    visible: ctxRelayMgmt.dmRelays.length == 0
                }

                ListView {
                    id: messageListView
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    clip: true
                    verticalLayoutDirection: ListView.TopToBottom
                    layoutDirection: Qt.LeftToRight
                    leftMargin: Constants.spacing_m
                    rightMargin: Constants.spacing_m
                    visible: ctxRelayMgmt.dmRelays.length > 0

                    model: AutoListModel {
                        id: messagesModel
                        source: privateMessages
                        mode: AutoListModel.ByKey
                        equalityTest: function (oldItem, newItem) {
                            return oldItem.content === newItem.content
                                && oldItem.isOwnMessage === newItem.isOwnMessage
                                && oldItem.timestamp === newItem.timestamp
                        }
                    }

                    delegate: Loader {
                        active: modelData !== undefined && modelData !== null
                        width: messageListView.width - messageListView.leftMargin - messageListView.rightMargin - 15
                        height: active ? item.height : 0

                        sourceComponent: Item {
                            property var message: modelData
                            height: privateRowLayout.height + 8

                            RowLayout {
                                id: privateRowLayout
                                width: parent.width
                                spacing: Constants.spacing_m
                                y: Constants.spacing_m
                                layoutDirection: message ? (message.isOwnMessage ? Qt.RightToLeft : Qt.LeftToRight) : Qt.LeftToRight

                                ProfilePicture {
                                    imageSource: message ? (message.isOwnMessage ?
                                        Util.getProfilePicture(mypicture, mynpub) :
                                        Util.getProfilePicture(profileData.picture, profileData.npub)) : ""
                                }

                                Pane {
                                    Layout.fillWidth: true
                                    Layout.maximumWidth: parent.width * 0.7
                                    Layout.rightMargin: message ? (message.isOwnMessage ? 0 : Constants.spacing_m) : Constants.spacing_m
                                    Layout.leftMargin: message ? Constants.spacing_m : 0
                                    padding: Constants.spacing_m
                                    background: Rectangle {
                                        color: message ? (message.isOwnMessage ? Material.accentColor : Material.dividerColor) : Material.dividerColor
                                        radius: 10
                                    }

                                    ColumnLayout {
                                        spacing: Constants.spacing_s
                                        width: parent.width

                                        Text {
                                            Layout.fillWidth: true
                                            text: message ? message.content : ""
                                            wrapMode: Text.Wrap
                                            color: Material.foreground
                                        }

                                        Text {
                                            Layout.alignment: message ? (message.isOwnMessage ? Qt.AlignRight : Qt.AlignLeft) : Qt.AlignLeft
                                            text: message ? message.timestamp : ""
                                            font: Constants.smallFontMedium
                                            color: Material.secondaryTextColor
                                            opacity: 0.9
                                        }
                                    }
                                }
                            }
                        }
                    }

                    onCountChanged: {
                        if (atYEnd || count === 1) {
                            positionViewAtEnd()
                        }
                    }

                    onContentHeightChanged: {
                        positionViewAtEnd()
                    }

                    Component.onCompleted: {
                        positionViewAtEnd()
                    }

                    ScrollBar.vertical: ScrollBar {
                        active: true
                        policy: ScrollBar.AsNeeded
                    }
                }

                // Private Messages Input
                MessageInput {
                    placeholderText: qsTr("Type a message...")
                    buttonText: qsTr("Send")
                    onMessageSent: function(text) {
                        sendPrivateMessage(text)
                    }
                }
            }
        }
    }

    // Add loading indicator
    BusyIndicator {
        id: loadingIndicator
        anchors.centerIn: parent
        visible: false
        running: visible
        z: 1000
    }

    // Add error banner
    Rectangle {
        id: errorBanner
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        height: errorText.height + 2 * Constants.spacing_m
        color: Material.color(Material.Red)
        visible: false
        z: 1000

        Text {
            id: errorText
            anchors.centerIn: parent
            color: "white"
            font: Constants.fontMedium
        }

        function showError(message) {
            errorText.text = message
            visible = true
            errorTimer.restart()
        }

        Timer {
            id: errorTimer
            interval: 3000
            onTriggered: errorBanner.visible = false
        }
    }

    // Add states for UI elements
    states: [
        State {
            name: "loading"
            PropertyChanges { target: loadingIndicator; visible: true }
            PropertyChanges { target: mainContentArea; opacity: 0.5 }
        },
        State {
            name: "error"
            PropertyChanges { target: errorBanner; visible: true }
        },
        State {
            name: "ready"
            PropertyChanges { target: mainContentArea; opacity: 1.0 }
        }
    ]

    transitions: [
        Transition {
            from: "*"; to: "*"
            NumberAnimation {
                properties: "opacity"
                duration: 150
                easing.type: Easing.InOutQuad
            }
        }
    ]
}

